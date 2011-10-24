
package de.jetho.pl0compiler

import scala.io.Source._
import java.io.{ FileOutputStream, DataOutputStream }


object PL0Compiler {

  val usage = """
    Usage: scala PL0Compiler [-i] srcfile [destfile]
  """

  def readFile(file: String): Option[String] = 
    try {
      Some(fromFile(file).getLines.mkString("\n"))
    } catch {
      case e: Exception => Console.err.println(e)
                           None
    }

  def writeOutputFile(outfile: String, code: List[Instruction]) =
    try {
      val objectFile = new FileOutputStream (outfile)
      val objectStream = new DataOutputStream (objectFile)

      code.foreach { 
        case Instruction(op, r, n, d) => { 
          objectStream.writeInt(op)
          objectStream.writeInt(r)
          objectStream.writeInt(n)
          objectStream.writeInt(d)
        }
      }
      objectFile.close
    } catch {
      case e: Exception => Console.err.println(e)
    }


  // use Failure Monad to check and interpret the program
  def interpret(file: String) =
    for { 
      src  <- readFile(file) 
      ast  <- PL0Parser.parse(src)
      _    <- Semant.check(ast)      
    } TreeInterpreter.eval(ast)    
    

  // use Failure Monad to check and compile the program
  def compile(file: String, outfile: String) = 
    for {
      src  <- readFile(file)
      ast  <- PL0Parser.parse(src)
      _    <- Semant.check(ast)
      code <- CodeGenerator.encode(ast)
    } writeOutputFile(outfile, code)


  def main(args: Array[String]) {    
    args.toList match {
      case "-i" :: srcfile :: Nil => interpret(srcfile)
      case srcfile :: destfile :: Nil => compile(srcfile, destfile)
      case _ => println(usage)
                sys.exit(1)
    }
  }
    
}
