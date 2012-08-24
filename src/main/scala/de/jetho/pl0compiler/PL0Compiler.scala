
/** The main module.*/


package de.jetho.pl0compiler

import scala.io.Source._
import java.io.{ FileOutputStream, DataOutputStream }
import scalaz._
import Scalaz._


object PL0Compiler {

  def usage = """
    Usage: scala PL0Compiler [-i] srcfile [destfile]
  """


  def printErrors = (errors: NonEmptyList[String]) => errors.foreach(println)


  def readFile(file: String): Validation[String, String] = 
    try {
      fromFile(file).mkString.success
    } catch { case e: Exception => e.toString.fail }


  /** generate object file.*/
  def writeOutputFile(outfile: String, code: List[Instruction]) = {
    val objectFile = new FileOutputStream (outfile)
    try {
      val objectStream = new DataOutputStream (objectFile)
      code.foreach { 
        case Instruction(op, r, n, d) => { 
          objectStream.writeInt(op)
          objectStream.writeInt(r)
          objectStream.writeInt(n)
          objectStream.writeInt(d)
        }
      }      
    } finally { objectFile.close }
  }


  /** execute the front end parts.*/
  def parseAndCheck(file: String): ValidationNEL[String, AST] =
    for {
      src <- readFile(file).liftFailNel
      ast <- PL0Parser.parse(src).liftFailNel
      _   <- Semant.check(ast)
    } yield ast


  /** analyze and interpret the program.*/
  def interpret(file: String) =
    parseAndCheck(file).fold(printErrors, TreeInterpreter.eval(_))
 

  def compile(file: String, outfile: String) = {
    val result = parseAndCheck(file)
    result.fold(printErrors)
  }

  
  def main(args: Array[String]) {    
    args.toList match {
      case "-i" :: srcfile :: Nil => interpret(srcfile)
      case srcfile :: destfile :: Nil => compile(srcfile, destfile)
      case _ => println(usage)
                sys.exit(1)
    }
  }
    
}
