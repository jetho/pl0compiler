

package org.jetho.pl0compiler


import scalaz._
import Scalaz._
import StateT._


object CodeGenerator {

  type RuntimeEnvironment = Environment[RuntimeEntity]
  type CodeBlock = (Int, DList[Instruction])
  type Result[+X] = \/[String, X]
  type StateTEither[+X] = StateT[Result, Int, X]


  /** generate the code and patch the forward references.*/
  def encode(ast: AST): \/[String, List[Instruction]] =      
    for { 
      (len, code) <- encodeProgram(ast) eval 0
      _ <- checkLength(len)
      patchedCode <- patchCode(code.toList)
    } yield patchedCode
  
  
  /** some helper functions */

  def fail[A](msg: String) = 
    StateT[Result, Int, A](_ => msg.left[(Int, A)])

  /** generate a Diff List for an instruction with length 1.*/
  def emit(op: Int, n: Int, r: Int, d: Int): StateTEither[CodeBlock] = 
    stateT((1, DList(Instruction(op, r, n, d))))

  /** generate an instruction and increment the address counter.*/
  def emitAndIncr(op: Int, n: Int, r: Int, d: Int): StateTEither[CodeBlock] = 
    incrInstrCounter *> emit(op, n, r, d)
  
   /** register resolution for nested prodedure calls.*/
  def displayRegister(currentLevel: Int, objectLevel: Int): StateTEither[Int] =
    objectLevel match { 
      case 0 => stateT(Instruction.rSB)
      case _ if (currentLevel - objectLevel <= 6) => stateT(Instruction.rLB + currentLevel - objectLevel)
      case _ => fail("Can't access data more than 6 levels out!")
    }

  /** generate an empty instruction.*/
  def skip: StateTEither[CodeBlock] = stateT((0, DList()))

  def sum(ints: Int*) = ints.sum 

  /** update the state (address counter) in the monad.*/
  def updateInstrCounter(i: Int) = 
    StateT[Result, Int, Unit](s => (i, ()).point[Result])

  def getInstrCounter =
    StateT[Result, Int, Int](s => (s, s).point[Result])

  def incrInstrCounter =
    StateT[Result, Int, Unit](s => (s+1, ()).point[Result])

  def checkLength(len: Int) = 
    (len > Instruction.PB) either ("Can't process more than " + Instruction.PB + " Instructions!") or len
  
  /* patch the unresolved forward references.*/
  def patchCode(code: List[Instruction]) = {
    def patch(i: Instruction) = i.success[String]
    
    /* patch the instructions; collect errors by sequencing Validations;
       convert the result to an Either type.*/
    code.map(patch(_).toValidationNEL)
        .sequenceU
        .bimap(_.toList.mkString("\n"), identity)
        .disjunction
  }
  

  /* the encoding functions for the various AST nodes.*/
  
  def encodeProgram(ast: AST): StateTEither[CodeBlock] = {
    val globalFrame = Frame(0, 0)
    val globalEnv = EmptyEnvironment[RuntimeEntity].extend(primitiveRoutines())
    for {
      (l1, code) <- encodeAst(ast, globalEnv, globalFrame)
      (l2, halt) <- emit(Instruction.opHALT, 0, 0, 0)
    } yield ( sum(l1, l2), code ++ halt )
  }

  
  def encodeAst(ast: AST, env: RuntimeEnvironment, frame: Frame): StateTEither[CodeBlock] = 
    ast match {

      case BinaryCondition(cond, left, right) => 
        for {
          (l1, c1) <- encodeAst(left, env, frame)
          (l2, c2) <- encodeAst(right, env, frame)
          (l3, c3) <- if (cond == "=" || cond == "#") emitAndIncr(Instruction.opLOADL, 0, 0, 1)
                      else skip
          (l4, c4) <- encodeAst(CallStmt(cond), env, frame)
        } yield ( sum(l1, l2, l3 ,l4), 
                  c1 ++ c2 ++ c3 ++ c4 )

      case OddCondition(expr) => 
        for {
          (l1, c1) <- encodeAst(expr, env, frame)
          (l2, c2) <- emitAndIncr(Instruction.opLOADL, 0, 0, 2)
          (l3, c3) <- encodeAst(CallStmt("$mod"), env, frame)
          (l4, c4) <- emitAndIncr(Instruction.opLOADL, 0, 0, 0)
          (l5, c5) <- emitAndIncr(Instruction.opLOADL, 0, 0, 1)
          (l6, c6) <- encodeAst(CallStmt("#"), env, frame)
        } yield ( sum(l1, l2, l3, l4, l5, l6), 
                  c1 ++ c2 ++ c3 ++ c4 ++ c5 ++ c6 )

      case BinOp(op, left, right) => 
        for {
          (l1, c1) <- encodeAst(left, env, frame)
          (l2, c2) <- encodeAst(right, env, frame)
          (l3, c3) <- encodeAst(CallStmt(op), env, frame)
        } yield ( sum(l1, l2, l3), c1 ++ c2 ++ c3 )

      case Ident(name) => 
        env.resolve(name) match {
          case Some(Constant(value)) => emitAndIncr(Instruction.opLOADL, 0, 0, value)
          case Some(Variable(EntityAddress(addressLevel, displacement))) => 
            for {
              reg      <- displayRegister(frame.level, addressLevel)
              (l1, c1) <- emitAndIncr(Instruction.opLOAD, 1, reg, displacement)
            } yield (l1, c1)
          case _ => fail("Unresolved Variable " + name)
        }

      case IntLiteral(value) => emitAndIncr(Instruction.opLOADL, 0, 0, value)

      case _ => fail("Unknown AST-Node: " + ast)    
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



