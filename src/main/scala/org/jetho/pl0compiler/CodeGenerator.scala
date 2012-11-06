
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

  type CodeBlock = (Int, DList[Instruction])
  type Result[+X] = \/[String, X]
  type StateTEither[+X] = StateT[Result, Int, X]


  /** generate the code and patch the forward references.*/
  def encodeAst(ast: AST): \/[String, List[Instruction]] =      
    for { 
      (len, code) <- encodeProgram(ast) eval 0
      _ <- checkLength(len)
      patchedCode <- patchCode(code.toList)
    } yield patchedCode
  
  
  /** some helper functions */
  
  def fail[A](msg: String) = 
    StateT[Result, Int, A](_ => msg.left[(Int, A)])
  
  def sum(ints: Int*) = ints.sum

  def merge(dlists: DList[Instruction]*) = dlists.fold(DList[Instruction]())( _ ++ _)

  
  /** functions for modifying the state.*/

  def updateInstrCounter(i: Int) = 
    StateT[Result, Int, Unit](s => (i, ()).point[Result])

  def getInstrCounter =
    StateT[Result, Int, Int](s => (s, s).point[Result])

  def incrInstrCounter =
    StateT[Result, Int, Unit](s => (s+1, ()).point[Result])


  /** helper functions for the bytecode generation.*/
 
  /** generate a Diff List for an instruction with length 1.*/
  def emit(op: Int, n: Int, r: Int, d: Int, i: Option[String]=None, e: Option[RE]=None, f: Option[Frame]=None) = 
    stateT[Result, Int, CodeBlock]( (1, DList(Instruction(op, r, n, d, i, e, f))) )

  /** generate an instruction and increment the address counter.*/
  def emitAndIncr(op: Int, n: Int, r: Int, d: Int, i: Option[String]=None, e: Option[RE]=None, f: Option[Frame]=None) = 
    incrInstrCounter >> emit(op, n, r, d, i, e, f)  

  /** generate an empty instruction.*/
  def skip: StateTEither[CodeBlock] = stateT((0, DList()))

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

  /* patch the unresolved forward references.*/
  def patchCode(code: List[Instruction]) = {
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
    
    code.map(patch(_).validation.toValidationNEL)
        .sequenceU
        .bimap(_.toList.mkString("\n"), identity)
        .disjunction
  }
  

  /* the encoding functions for the various AST nodes.*/
  
  def encodeProgram(ast: AST): StateTEither[CodeBlock] = {
    val globalFrame = Frame(0, 0)
    val globalEnv = EmptyEnvironment[RuntimeEntity].extend(primitiveRoutines())
    for {
      (l1, code) <- encode(ast, globalEnv, globalFrame)
      (l2, halt) <- emit(Instruction.opHALT, 0, 0, 0)
    } yield ( sum(l1, l2), merge(code, halt) )
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
          (l1, c1) <- if (varBindings.length > 0) emitAndIncr(Instruction.opPUSH, 0, 0, varBindings.length)
                      else skip
          resList  <- procDecls.map( encode(_, newLexicalEnvironment, frame) ).sequenceU
          val (l2, c2) = (sum(resList.map(_._1) :_*), merge(resList.map(_._2) :_*)) 
          (l3, c3) <- statement.map(encode(_, newLexicalEnvironment, frame) ).getOrElse(skip)
          (l4, c4) <- if (varBindings.length > 0) emitAndIncr(Instruction.opPOP, 0, 0, varBindings.length)
                      else skip
        } yield ( sum(l1, l2, l3, l4), 
                  merge(c1, c2, c3, c4) )


      case SeqStmt(stmts) => 
        for {
          resList <- stmts.map( encode(_, env, frame) ).sequenceU 
        } yield ( sum(resList.map(_._1) :_*), 
                  merge(resList.map(_._2) :_*) )


      case CallStmt(ident) => 
        env.resolve(ident) match { 
          case Some(Proc(None)) =>
            for {
              (l, cdummy) <- emitAndIncr(Instruction.opCALL_DUMMY, 0, 0, 0, ident.some, env.some, frame.some)
            } yield (l, cdummy)
          case Some(proc) => 
	        encodeRoutineCall(ident, proc, frame)
          case _ => fail("Unkown routine " + ident)
        }


      case AssignStmt(ident, expr) => 
        for {
          (l1, c1) <- encode(expr, env, frame)
          (l2, c2) <- env.resolve(ident) match { 
            case Some(Variable(EntityAddress(addressLevel, displacement))) =>
              for {
                reg    <- displayRegister(frame.level, addressLevel)
                (l, c) <- emitAndIncr(Instruction.opSTORE, 1, reg , displacement)
              } yield (l, c)
            case _ => fail("Unresolved Variable in Assignment: " + ident)   
          }          
        } yield ( sum(l1, l2), merge(c1, c2) )


      case PrintStmt(expr) => 
        for {
          (l1, c1) <- encode(expr, env, frame)
          (l2, c2) <- encode(CallStmt("!"), env, frame)
          (l3, c3) <- encode(CallStmt("$puteol"), env, frame)
        } yield ( sum(l1, l2, l3), merge(c1, c2, c3) )


      case BinaryCondition(cond, left, right) => 
        for {
          (l1, c1) <- encode(left, env, frame)
          (l2, c2) <- encode(right, env, frame)
          (l3, c3) <- if (cond == "=" || cond == "#") emitAndIncr(Instruction.opLOADL, 0, 0, 1)
                      else skip
          (l4, c4) <- encode(CallStmt(cond), env, frame)
        } yield ( sum(l1, l2, l3 ,l4), 
                  merge(c1, c2, c3, c4) )


      case OddCondition(expr) => 
        for {
          (l1, c1) <- encode(expr, env, frame)
          (l2, c2) <- emitAndIncr(Instruction.opLOADL, 0, 0, 2)
          (l3, c3) <- encode(CallStmt("$mod"), env, frame)
          (l4, c4) <- emitAndIncr(Instruction.opLOADL, 0, 0, 0)
          (l5, c5) <- emitAndIncr(Instruction.opLOADL, 0, 0, 1)
          (l6, c6) <- encode(CallStmt("#"), env, frame)
        } yield ( sum(l1, l2, l3, l4, l5, l6), 
                  merge(c1, c2, c3, c4, c5, c6) )


      case BinOp(op, left, right) => 
        for {
          (l1, c1) <- encode(left, env, frame)
          (l2, c2) <- encode(right, env, frame)
          (l3, c3) <- encode(CallStmt(op), env, frame)
        } yield ( sum(l1, l2, l3), merge(c1, c2, c3) )


      case Ident(name) => 
        env.resolve(name) match {
          case Some(Constant(value)) => emitAndIncr(Instruction.opLOADL, 0, 0, value)
          case Some(Variable(EntityAddress(addressLevel, displacement))) => 
            for {
              reg    <- displayRegister(frame.level, addressLevel)
              (l, c) <- emitAndIncr(Instruction.opLOAD, 1, reg, displacement)
            } yield (l, c)
          case _ => fail("Unresolved Variable " + name)
        }


      case IntLiteral(value) => emitAndIncr(Instruction.opLOADL, 0, 0, value)


      case _ => fail("Unknown AST-Node: " + ast)    
    }

  
  /** encode procedure calls.*/
  def encodeRoutineCall(ident: String, proc: RuntimeEntity, frame: Frame): StateTEither[CodeBlock] = 
    proc match {    
      case Proc(Some(EntityAddress(addressLevel, displacement))) =>  
        for {
           reg    <- displayRegister(frame.level, addressLevel)
           (l, c) <- emitAndIncr(Instruction.opCALL, reg, Instruction.rCB, displacement)
        } yield (l, c) 
      case PrimitiveRoutine(displacement) => 
        emitAndIncr(Instruction.opCALL, Instruction.rSB, Instruction.rPB, displacement) 
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



