
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


  def printErrors = (errors: List[String]) => errors.foreach(println)


  def readFile(file: String): \/[List[String], String] = 
    try {
      fromFile(file).mkString.right
    } catch { case e: Exception => List(e.toString).left }


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
  def parseAndAnalyze(file: String): \/[List[String], AST] =
    for {
      src <- readFile(file)
      ast <- PL0Parser.parse(src)
      _   <- Semant.analyze(ast)
    } yield ast


  /** analyze and interpret the program.*/
  def interpret(file: String) =
    parseAndAnalyze(file).fold(printErrors, TreeInterpreter.eval(_))
 

//  def compile(file: String, outfile: String) = {
//    val result = parseAndAnalyze(file)
//    result.fold(printErrors)
//  }

  
  def main(args: Array[String]) {    
    args.toList match {
      case "-i" :: srcfile :: Nil => interpret(srcfile)
    //  case srcfile :: destfile :: Nil => compile(srcfile, destfile)
      case _ => println(usage)
                sys.exit(1)
    }
  }
    
}
