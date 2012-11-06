
/** The Code Generator emits bytecode for the TAM Abstract Machine.
    A State Monad Transformer is used to avoid excessive backpatching.
    The List of Instructions is built by merging small difference lists
    containing single instructions.*/



package org.jetho.pl0compiler


import scalaz._
import Scalaz._
import StateT._


object CodeGenerator {

  type RuntimeEnvironment = Environment[RuntimeEntity]
  type RE = RuntimeEnvironment

  type CodeBlock = DList[Instruction]
  type Result[+X] = \/[String, X]
  type StateTEither[+X] = StateT[Result, Int, X]


  /** generate the code and patch the forward references.*/
  def encodeAst(ast: AST): \/[String, List[Instruction]] =      
    for { 
      codeDList <- encodeProgram(ast) eval Instruction.CB
      val code = codeDList.toList
      _ <- checkLength(code.length)
      patchedCode <- patchCode(code).disjunction
    } yield patchedCode
  
  
  /** some helper functions */
  
  def fail[A](msg: String) = 
    StateT[Result, Int, A](_ => msg.left[(Int, A)])
  
  /** concat a list of DLists.*/
  def merge(dlists: DList[Instruction]*) = dlists.fold(DList[Instruction]())( _ ++ _)

  
  /** functions for handling the state in the state monad.*/

  def getInstrCounter =
    StateT[Result, Int, Int](s => (s, s).point[Result])

  def incrInstrCounter =
    StateT[Result, Int, Int](s => (s+1, s+1).point[Result])


  /** helper functions for the bytecode generation.*/
 
  /** generate a Diff List for an instruction with length 1.*/
  def instr(op: Int, n: Int, r: Int, d: Int, i: Option[String]=None, e: Option[RE]=None, f: Option[Frame]=None) = 
    stateT[Result, Int, CodeBlock]( DList(Instruction(op, r, n, d, i, e, f)) )

  /** generate an instruction and increment the address counter.*/
  def emit(op: Int, n: Int, r: Int, d: Int, i: Option[String]=None, e: Option[RE]=None, f: Option[Frame]=None) = 
    incrInstrCounter >> instr(op, n, r, d, i, e, f)  

  /** generate an empty instruction.*/
  def skip: StateTEither[CodeBlock] = stateT(DList())

  /** register resolution for nested prodedure calls.*/
  def displayRegister(currentLevel: Int, objectLevel: Int): StateTEither[Int] =
    objectLevel match { 
      case 0 => stateT(Instruction.rSB)
      case _ if (currentLevel - objectLevel <= 6) => stateT(Instruction.rLB + currentLevel - objectLevel)
      case _ => fail("Can't access data more than 6 levels out!")
    }

 
  /** functions for validating and patching the generated bytecode.*/
  
  def checkLength(len: Int) = 
    (len > Instruction.PB) either ("Can't process more than " + Instruction.PB + " Instructions!") or len

  /** patch a dummy procedure call consulting the constructed runtime environment.*/
  def patch(i: Instruction): \/[String, Instruction] = i match {
    case Instruction(Instruction.opCALL_DUMMY, n, _, _, Some(id), Some(env), Some(frame)) =>
      env.resolve(id) match {
        case Some(Proc(Some(EntityAddress(addressLevel, displacement)))) => 
          for {
            reg <- (displayRegister(frame.level, addressLevel) eval 0)
          } yield Instruction(Instruction.opCALL, n, reg, displacement)
        case _ => ("Unresolved Procedure: " + id).left
      }
    case Instruction(Instruction.opCALL_DUMMY, n, _, _, _, _, _) =>
      ("Incomplete Patching Information for Procedure: " + id).left
    case instr => instr.right
  }   

  /* patch all unresolved forward references.*/
  def patchCode(code: List[Instruction]) = 
    code.map(patch(_).validation.toValidationNEL)           // validate/patch all instructions
        .sequenceU                                          // sequence the validations
        .bimap(_.toList.mkString("\n"), identity)           // merge errors or return the patched code
       

  /* the encoding functions for the various AST nodes.*/
  
  def encodeProgram(ast: AST): StateTEither[CodeBlock] = {
    val globalFrame = Frame(0, 0)
    val globalEnv = EmptyEnvironment[RuntimeEntity].extend(primitiveRoutines())
    for {
      code  <- encode(ast, globalEnv, globalFrame)
      halt  <- instr(Instruction.opHALT, 0, 0, 0)
    } yield merge(code, halt)
  }

  
  def encode(ast: AST, env: RE, frame: Frame): StateTEither[CodeBlock] = 
    ast match {

      case Block(constDecls, varDecls, procDecls, statement) => 
        val constBindings = constDecls.map { case ConstDecl(id, num) => (id, Constant(num)) }
        val varBindings = varDecls.zipWithIndex.map { case (decl, idx) => 
          (decl.ident, Variable(EntityAddress(frame.level, frame.offset + idx))) }
        val procBindings = procDecls.map { procDecl => (procDecl.ident, Proc(None)) }
        val newLexicalEnvironment = env.extend( constBindings ::: varBindings ::: procBindings )

        for {
          c1    <- if (varBindings.length > 0) emit(Instruction.opPUSH, 0, 0, varBindings.length)
                   else skip
          clist <- procDecls.map( encode(_, newLexicalEnvironment, frame) ).sequenceU
          val c2 = merge(clist :_*) 
          c3    <- statement.map(encode(_, newLexicalEnvironment, frame) ).getOrElse(skip)
          c4    <- if (varBindings.length > 0) emit(Instruction.opPOP, 0, 0, varBindings.length)
                   else skip
        } yield merge(c1, c2, c3, c4)
      

      case ProcDecl(ident, block) => 
        for {
          a0    <- incrInstrCounter
          c2    <- encode(block, env, Frame(frame.level + 1, 3))
          c3    <- emit(Instruction.opRETURN, 0, 0, 0)
          addr  <- getInstrCounter
          c1    <- instr(Instruction.opJUMP, 0, Instruction.rCB, addr)
        } yield { env.update(ident, Proc(Some(EntityAddress(frame.level, a0))))
                  merge(c1, c2, c3) }
                

      case SeqStmt(stmts) => 
        for {
          clist <- stmts.map( encode(_, env, frame) ).sequenceU 
        } yield merge(clist :_*)


      case CallStmt(ident) => 
        env.resolve(ident) match { 
          case Some(Proc(None)) => emit(Instruction.opCALL_DUMMY, 0, 0, 0, ident.some, env.some, frame.some)
          case Some(proc) => encodeRoutineCall(ident, proc, frame)
          case _ => fail("Unkown routine " + ident)
        }


      case AssignStmt(ident, expr) => 
        for {
          c1    <- encode(expr, env, frame)
          c2    <- env.resolve(ident) match { 
            case Some(Variable(EntityAddress(addressLevel, displacement))) =>
              for {
                reg <- displayRegister(frame.level, addressLevel)
                c   <- emit(Instruction.opSTORE, 1, reg , displacement)
              } yield c
            case _ => fail("Unresolved Variable in Assignment: " + ident)   
          }          
        } yield merge(c1, c2)


      case IfStmt(condition, stmt) => 
        for {
          c1    <- encode(condition, env, frame)
          _     <- incrInstrCounter
          c3    <- stmt.map { encode(_, env, frame) }.getOrElse(skip)
          end   <- getInstrCounter
          c2    <- instr(Instruction.opJUMPIF, 0, Instruction.rCB, end)
        } yield merge(c1, c2, c3)


      case WhileStmt(condition, stmt) => 
        for {
          start <- incrInstrCounter 
          c2    <- stmt.map { encode(_, env, frame) }.getOrElse(skip)
          after <- getInstrCounter
          c1    <- instr(Instruction.opJUMP, 0, Instruction.rCB, after)
          c3    <- encode(condition, env, frame)
          c4    <- emit(Instruction.opJUMPIF, 1, Instruction.rCB, start)
        } yield merge(c1, c2, c3, c4)


      case PrintStmt(expr) => 
        for {
          c1    <- encode(expr, env, frame)
          c2    <- encode(CallStmt("!"), env, frame)
          c3    <- encode(CallStmt("$puteol"), env, frame)
        } yield merge(c1, c2, c3)


      case BinaryCondition(cond, left, right) => 
        for {
          c1    <- encode(left, env, frame)
          c2    <- encode(right, env, frame)
          c3    <- if (cond == "=" || cond == "#") emit(Instruction.opLOADL, 0, 0, 1)
                   else skip
          c4    <- encode(CallStmt(cond), env, frame)
        } yield merge(c1, c2, c3, c4)


      case OddCondition(expr) => 
        for {
          c1    <- encode(expr, env, frame)
          c2    <- emit(Instruction.opLOADL, 0, 0, 2)
          c3    <- encode(CallStmt("$mod"), env, frame)
          c4    <- emit(Instruction.opLOADL, 0, 0, 0)
          c5    <- emit(Instruction.opLOADL, 0, 0, 1)
          c6    <- encode(CallStmt("#"), env, frame)
        } yield merge(c1, c2, c3, c4, c5, c6)


      case BinOp(op, left, right) => 
        for {
          c1    <- encode(left, env, frame)
          c2    <- encode(right, env, frame)
          c3    <- encode(CallStmt(op), env, frame)
        } yield merge(c1, c2, c3)


      case Ident(name) => 
        env.resolve(name) match {
          case Some(Constant(value)) => emit(Instruction.opLOADL, 0, 0, value)
          case Some(Variable(EntityAddress(addressLevel, displacement))) => 
            for {
              reg  <- displayRegister(frame.level, addressLevel)
              c    <- emit(Instruction.opLOAD, 1, reg, displacement)
            } yield c
          case _ => fail("Unresolved Variable " + name)
        }


      case IntLiteral(value) => emit(Instruction.opLOADL, 0, 0, value)


      case _ => fail("Unknown AST-Node: " + ast)    
    }

  
  /** encode procedure calls.*/
  def encodeRoutineCall(ident: String, proc: RuntimeEntity, frame: Frame): StateTEither[CodeBlock] = 
    proc match {    
      case Proc(Some(EntityAddress(addressLevel, displacement))) =>  
        for {
           reg  <- displayRegister(frame.level, addressLevel)
           c    <- emit(Instruction.opCALL, reg, Instruction.rCB, displacement)
        } yield c 
      case PrimitiveRoutine(displacement) => emit(Instruction.opCALL, Instruction.rSB, Instruction.rPB, displacement) 
      case _ => fail("Unknown Routine " + ident)
    }
  

  /** addresses of the primitive routines.*/
  def primitiveRoutines(): List[(String, PrimitiveRoutine)] =  
    List(
      ("!", PrimitiveRoutine(Instruction.putintDisplacement)),
      ("$puteol", PrimitiveRoutine(Instruction.puteolDisplacement)),
      ("-", PrimitiveRoutine(Instruction.subDisplacement)),
      ("+", PrimitiveRoutine(Instruction.addDisplacement)),
      ("*", PrimitiveRoutine(Instruction.multDisplacement)),
      ("/", PrimitiveRoutine(Instruction.divDisplacement)),
      ("$mod", PrimitiveRoutine(Instruction.modDisplacement)),
      ("<", PrimitiveRoutine(Instruction.ltDisplacement)),
      ("<=", PrimitiveRoutine(Instruction.leDisplacement)),
      (">=", PrimitiveRoutine(Instruction.geDisplacement)),
      (">", PrimitiveRoutine(Instruction.gtDisplacement)),
      ("=", PrimitiveRoutine(Instruction.eqDisplacement)),
      ("#", PrimitiveRoutine(Instruction.neDisplacement))     
    )

}



