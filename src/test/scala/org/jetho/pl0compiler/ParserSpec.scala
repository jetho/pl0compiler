

package org.jetho.pl0compiler


import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import scalaz._
import Scalaz._


class ParserSpec extends FlatSpec with ShouldMatchers {

  def parse(s: String) = PL0Parser.parse(s)

  "The Parser" should "parse an empty program just containing an end dot" in {
  	parse(".") should equal (Block(List(), List(), List(), None).right)    
  }

  it should "parse an empty block" in {
  	parse("BEGIN END.") should equal (Block(List(), List(), List(), Some(SeqStmt(List()))).right)
  }

  it should "parse a block containing single or multiple statements" in {
  	parse("BEGIN x := 1 END.") should equal (
  		Block(List(), List(), List(), Some(SeqStmt(List(AssignStmt("x", IntLiteral(1)))))).right)
  	
  	parse("BEGIN x := 1; y := x+x END.") should equal (
  		Block(List(),List(),List(),Some(SeqStmt(List(AssignStmt("x",IntLiteral(1)), AssignStmt("y",BinOp("+",Ident("x"),Ident("x"))))))).right)
  }

  it should "correctly parse arithmetic expressions" in {
  	parse("BEGIN !1+1+1 END.") should equal (
  		Block(List(),List(),List(),Some(SeqStmt(List(PrintStmt(BinOp("+",BinOp("+",IntLiteral(1),IntLiteral(1)),IntLiteral(1))))))).right)
  	
  	parse("BEGIN !1-2-3-4 END.") should equal (
  		Block(List(),List(),List(),Some(SeqStmt(List(PrintStmt(BinOp("-",BinOp("-",BinOp("-",IntLiteral(1),IntLiteral(2)),IntLiteral(3)),IntLiteral(4))))))).right)
  	
  	parse("BEGIN !1*2*3 END.") should equal (
  		Block(List(),List(),List(),Some(SeqStmt(List(PrintStmt(BinOp("*",BinOp("*",IntLiteral(1),IntLiteral(2)),IntLiteral(3))))))).right)
  	
  	parse("BEGIN !1/2/3 END.") should equal (
  		Block(List(),List(),List(),Some(SeqStmt(List(PrintStmt(BinOp("/",BinOp("/",IntLiteral(1),IntLiteral(2)),IntLiteral(3))))))).right)
	
	parse("BEGIN !1+2*3 END.") should equal (
		Block(List(),List(),List(),Some(SeqStmt(List(PrintStmt(BinOp("+",IntLiteral(1),BinOp("*",IntLiteral(2),IntLiteral(3)))))))).right)
	
	parse("BEGIN !(1+2)*3 END.") should equal (
		Block(List(),List(),List(),Some(SeqStmt(List(PrintStmt(BinOp("*",BinOp("+",IntLiteral(1),IntLiteral(2)),IntLiteral(3))))))).right)
  }

}