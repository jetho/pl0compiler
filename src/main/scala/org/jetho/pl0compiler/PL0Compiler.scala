
/** The main module.*/


package org.jetho.pl0compiler

import scala.io.Source._
import java.io.{ FileOutputStream, DataOutputStream }
import scalaz._
import Scalaz._


object PL0Compiler {

  def usage = """
    Usage: scala PL0Compiler [-i] srcfile [destfile]
  """


  def printErrors = println(_: String)


  def readFile(file: String): \/[String, String] = 
    try {
      fromFile(file).mkString.right
    } catch { case e: Exception => e.toString.left }


  /** generate object file.*/
  def writeOutputFile(outfile: String, code: List[Instruction]) = {
    val objectFile = new FileOutputStream (outfile)
    try {
      val objectStream = new DataOutputStream (objectFile)
      code.foreach { 
        instruction =>
          objectStream.writeInt(instruction.op)
          objectStream.writeInt(instruction.r)
          objectStream.writeInt(instruction.n)
          objectStream.writeInt(instruction.d)
        }
      } finally { objectFile.close }
    }


  /** execute the front end parts.*/
  def parseAndAnalyze(file: String): \/[String, AST] =
    for {
      src <- readFile(file)
      ast <- PL0Parser.parse(src)
      _   <- Semant.analyze(ast)
    } yield ast


  /** analyze and interpret the program.*/
  def interpret(file: String) =
    parseAndAnalyze(file).fold(printErrors, TreeInterpreter.eval(_))
 

  def compile(file: String, outfile: String) = {
    val result = for {
      ast   <- parseAndAnalyze(file)
      code  <- CodeGenerator.encodeAst(ast)
    } yield code
    
    result.fold(printErrors, writeOutputFile(outfile, _))
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
