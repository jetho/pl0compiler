
/** The parser for the PL/0 language. See the "grammar" file for the corresponding BNF representation.*/


package de.jetho.pl0compiler

import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scalaz._
import Scalaz._


object PL0Parser extends StandardTokenParsers {   
  
  lexical.reserved += ("CONST", "VAR", "PROCEDURE", "CALL", "ODD", "BEGIN", "END", "IF", "THEN", "WHILE", "DO")
  lexical.delimiters += (":=", "=", "#", "<", "<=", ">", ">=", "+", "-", "*", "/", "(", ")", "?", "!", ";", ",", ".") 


  def program: Parser[Block] = block <~ "." 

  def block: Parser[Block] = opt(constDecl) ~ opt(varDecl) ~ rep(procDecl) ~ statement ^^ { 
    case constDecls ~ varDecls ~ procDecls ~ statement => 
      Block(constDecls.getOrElse(List[ConstDecl]()), varDecls.getOrElse(List[VarDecl]()), procDecls, statement) 
  }      
      
  def constDecl: Parser[List[ConstDecl]] = "CONST" ~> rep1sep(ident ~ "=" ~ numericLit, ",") <~ ";" ^^ { 
    _.map( _ match { case id ~ _ ~ num => ConstDecl(id, num.toInt)}) 
  }

  def varDecl: Parser[List[VarDecl]] = "VAR" ~> rep1sep(ident, ",") <~ ";" ^^ { _.map( VarDecl(_)) }

  def procDecl: Parser[ProcDecl] = "PROCEDURE" ~> ident ~ ";" ~ block <~ ";" ^^ { case id ~ _ ~ block => ProcDecl(id, block) }                          

  def statement: Parser[Option[Statement]] = opt( ident ~ ":=" ~ expr ^^ { case id ~ _ ~ expr => AssignStmt(id, expr) }
                                                | "CALL" ~> ident ^^ { CallStmt(_) } 
                                                | "!" ~> expr ^^ { PrintStmt(_) } 
                                                | "BEGIN" ~> rep1sep(statement, ";") <~ "END" ^^ { stmts => SeqStmt(stmts.flatten) }
                                                | "IF" ~> condition ~ "THEN" ~ statement ^^ { case cond ~ _ ~ stmt => IfStmt(cond, stmt) }
                                                | "WHILE" ~> condition ~ "DO" ~ statement ^^ { case cond ~ _ ~ stmt => WhileStmt(cond, stmt) } )                            
                               
  def condition: Parser[Condition] = ( "ODD" ~> expr ^^ { OddCondition(_) }
                                     | expr ~ ("=" | "#" | "<" | "<=" | ">" | ">=") ~ expr ^^ { 
					 case exp1 ~ op ~ exp2 => BinaryCondition(op, exp1, exp2) } )

  def expr: Parser[Expr] = term ~ rep(("+"|"-") ~ term) ^^ { 
    case t ~ Nil => t 
    case t ~ l => l.foldLeft(t) {
      case (z, "+" ~ p) => BinOp("+", z, p)
      case (z, "-" ~ p) => BinOp("-", z, p)
    }
  }

  def term: Parser[Expr] = factor ~ rep(("*"|"/") ~ factor) ^^ { 
    case f ~ Nil => f 
    case f ~ l => l.foldLeft(f) {
      case (z, "*" ~ p) => BinOp("*", z, p)
      case (z, "/" ~ p) => BinOp("/", z, p)
    }
  }           

  def factor: Parser[Expr] = ( numericLit ^^ { s => IntLiteral(s.toInt) }
                             | "(" ~> expr <~ ")" 
                             | ident ^^ { Ident(_) } )


  def parse(code: String): Validation[String, AST] = {
    val tokens = new lexical.Scanner(code)
    phrase(program)(tokens) match {
      case Success(ast, _) => ast.success
      case e: NoSuccess => e.toString.fail
    }
  }

}
