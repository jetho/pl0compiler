

package org.jetho.pl0compiler


import scalaz._
import Scalaz._
import StateT._


object CodeGenerator {

  type RuntimeEnvironment = Environment[RuntimeEntity]
  type CodeBlock = (Int, DList[Instruction])
  type Result[+X] = \/[String, X]
  type StateTEither[+X] = StateT[Result, Int, X]


  /* generate the code and patch the forward references.*/
  def encode(ast: AST): \/[String, List[Instruction]] =      
    for { 
      (len, code) <- encodeProgram(ast) eval 0
      _ <- checkLength(len)
      patchedCode <- patchCode(code.toList)
    } yield patchedCode
  
  
  /* some helper functions */

  /* generate a Diff List for an instruction with length 1.*/
  def emit(op: Int, n: Int, r: Int, d: Int): StateTEither[CodeBlock] = 
    stateT((1, DList(Instruction(op, r, n, d))))

  def updateInstrCounter(i: Int) = 
    StateT[Result, Int, Unit](s => (i, ()).point[Result])

  def getInstrCounter =
    StateT[Result, Int, Int](s => (s, s).point[Result])

  def checkLength(len: Int) = 
    (len > Instruction.PB) either ("Can't process more than " + Instruction.PB + " Instructions!") or len
  
  /* patch the unresolved forward references.*/
  def patchCode(code: List[Instruction]) = {
    def patch(i: Instruction) = i.success[String]
    
    /* patch the instructions or collect resulting errors by sequencing Validations;
       convert the result to an Either type.*/
    code.map(patch(_).toValidationNEL)
        .sequenceU//[({type l[a]=ValidationNEL[String, a]})#l, Instruction]
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
    } yield (l1 + l2, code ++ halt)
  }


  def encodeAst(ast: AST, env: RuntimeEnvironment, frame: Frame): StateTEither[CodeBlock] = emit(Instruction.opHALT, 0, 0, 0)


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



