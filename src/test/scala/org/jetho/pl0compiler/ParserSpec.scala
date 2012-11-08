

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
  		Block(List(),List(),List(),Some(SeqStmt(List(PrintStmt(BinOp("-",BinOp("-",BinOp("-",IntLiteral(1),IntLiteral(2)),
          IntLiteral(3)),IntLiteral(4))))))).right)
  	
  	parse("BEGIN !1*2*3 END.") should equal (
  		Block(List(),List(),List(),Some(SeqStmt(List(PrintStmt(BinOp("*",BinOp("*",IntLiteral(1),IntLiteral(2)),IntLiteral(3))))))).right)
  	
  	parse("BEGIN !1/2/3 END.") should equal (
  		Block(List(),List(),List(),Some(SeqStmt(List(PrintStmt(BinOp("/",BinOp("/",IntLiteral(1),IntLiteral(2)),IntLiteral(3))))))).right)
	
	parse("BEGIN !1+2*3 END.") should equal (
		Block(List(),List(),List(),Some(SeqStmt(List(PrintStmt(BinOp("+",IntLiteral(1),BinOp("*",IntLiteral(2),IntLiteral(3)))))))).right)
	
	parse("BEGIN !(1+2)*3 END.") should equal (
		Block(List(),List(),List(),Some(SeqStmt(List(PrintStmt(BinOp("*",BinOp("+",IntLiteral(1),IntLiteral(2)),IntLiteral(3))))))).right)

    parse("BEGIN !5 + 6 - 11 + 40 + 4 END.") should equal (
      Block(List(),List(),List(),Some(SeqStmt(List(PrintStmt(BinOp("+",BinOp("+",BinOp("-",
        BinOp("+",IntLiteral(5),IntLiteral(6)),IntLiteral(11)),IntLiteral(40)),IntLiteral(4))))))).right)
    }

    it should "parse simple statements" in {
      parse("BEGIN BEGIN !1; END; END.") should equal (
        Block(List(),List(),List(),Some(SeqStmt(List(SeqStmt(List(PrintStmt(IntLiteral(1)))))))).right)

      parse("BEGIN CALL x END.") should equal (Block(List(),List(),List(),Some(SeqStmt(List(CallStmt("x"))))).right)

      parse("BEGIN ! x END.") should equal (Block(List(),List(),List(),Some(SeqStmt(List(PrintStmt(Ident("x")))))).right)
    }

    it should "parse conditional statements" in {
      parse("BEGIN IF ODD x THEN !1 END.") should equal (Block(List(),List(),List(),Some(SeqStmt(List(IfStmt(
        OddCondition(Ident("x")),Some(PrintStmt(IntLiteral(1)))))))).right)
          
      parse("BEGIN WHILE x<10 DO BEGIN !x; x:=x+1; END; END.") should equal (
        Block(List(),List(),List(),Some(SeqStmt(List(WhileStmt(BinaryCondition("<",Ident("x"),IntLiteral(10)),
          Some(SeqStmt(List(PrintStmt(Ident("x")), AssignStmt("x",BinOp("+",Ident("x"),IntLiteral(1))))))))))).right)

      parse("BEGIN IF x>=1 THEN !x END.") should equal (
        Block(List(),List(),List(),Some(SeqStmt(List(IfStmt(BinaryCondition(">=",Ident("x"),IntLiteral(1)),
          Some(PrintStmt(Ident("x")))))))).right)
    }

    it should "parse declarations of constants, variables and procedures" in {
      parse("CONST x=1;.") should equal (Block(List(ConstDecl("x",1)),List(),List(),None).right)  
      
      parse("CONST x=1, y=2;.") should equal (Block(List(ConstDecl("x",1), ConstDecl("y",2)),List(),List(),None).right)

      parse("VAR x;.") should equal (Block(List(),List(VarDecl("x")),List(),None).right)  
      
      parse("VAR x, y;.") should equal (Block(List(),List(VarDecl("x"), VarDecl("y")),List(),None).right)

      parse("PROCEDURE a; !1; PROCEDURE b; !2;.") should equal (Block(List(),List(),List(ProcDecl("a",Block(List(),List(),List(),
        Some(PrintStmt(IntLiteral(1))))), ProcDecl("b",Block(List(),List(),List(),Some(PrintStmt(IntLiteral(2)))))),None).right)

      parse("CONST x=1, y=2; VAR w, z; PROCEDURE a; !1;PROCEDURE b; !2;.") should equal (Block(List(ConstDecl("x",1), ConstDecl("y",2)),
        List(VarDecl("w"), VarDecl("z")),List(ProcDecl("a",Block(List(),List(),List(),Some(PrintStmt(IntLiteral(1))))), ProcDecl("b",
          Block(List(),List(),List(),Some(PrintStmt(IntLiteral(2)))))),None).right)
    }

    it should "parse nested procedures and blocks" in {
      parse("PROCEDURE a; PROCEDURE b; CALL a; ;.") should equal (Block(List(),List(),List(ProcDecl("a",Block(List(),List(),List(
        ProcDecl("b",Block(List(),List(),List(),Some(CallStmt("a"))))),None))),None).right)

      parse("PROCEDURE a; PROCEDURE b; BEGIN CALL a; !1 END; BEGIN CALL b END;.") should equal (
        Block(List(),List(),List(ProcDecl("a",Block(List(),List(),List(ProcDecl("b",Block(List(),List(),List(),Some(SeqStmt(
          List(CallStmt("a"), PrintStmt(IntLiteral(1)))))))),Some(SeqStmt(List(CallStmt("b"))))))),None).right)
    }

    it should "parse numbers and identifiers" in {
      parse("!132423 .") should equal (Block(List(),List(),List(),Some(PrintStmt(IntLiteral(132423)))).right)
      parse("! _a3432 .") should equal (Block(List(),List(),List(),Some(PrintStmt(Ident("_a3432")))).right)
      parse("! XX123 .") should equal (Block(List(),List(),List(),Some(PrintStmt(Ident("XX123")))).right)
    }

    it should "reject illegal inputs" in {
      parse("").toEither should be ('left)
      parse("! 45s45 .").toEither should be ('left)
      parse("IF 1+1 THEN !a .").toEither should be ('left)
      parse("WHILE 1 DO !a .").toEither should be ('left)
      parse("BEGIN !a .").toEither should be ('left)
      parse("CONST; VAR x, y; .").toEither should be ('left)
      parse("CONST x,y; .").toEither should be ('left)
      parse("VAR x=1, y; .").toEither should be ('left)
      parse("PROCEDURE a !1; .").toEither should be ('left)
    }

    it should "accept legal programs" in {
      val s1 = """
        VAR f;

        PROCEDURE fact;
          VAR x, result;
          
          PROCEDURE fact1;
            BEGIN
              IF x <= f THEN
                BEGIN
                  result := result * x;
                  x := x + 1;
                  CALL fact1;
                END;
            END;
            
          BEGIN
            x := 1;
            result := 1;
            CALL fact1;
            ! result;
          END;

        BEGIN
          f := 6;  
          CALL fact;
        END.
      """
      parse(s1).toEither should be ('right)

      val s2 = """
        PROCEDURE proc1;
          PROCEDURE proc2;
            PROCEDURE proc3;
              CALL proc4;
            CALL proc3;
           
          PROCEDURE proc4;
            !7;
               
          CALL proc2;
         
        CALL proc1
        .
      """
      parse(s2).toEither should be ('right)

      val s3 = """
        PROCEDURE proc1;
          VAR x;  
          PROCEDURE proc2;
            VAR y;    
            PROCEDURE proc3;
              BEGIN
                x := 5;
                y := 6;
              END;
                
            BEGIN
              CALL proc3;
              !y
            END;    

          BEGIN
            CALL proc2;
            !x
          END;
         
        CALL proc1
        .
      """
      parse(s3).toEither should be ('right)

      val s4 = """
        VAR x, squ;
         
        PROCEDURE square;
          BEGIN
            squ := x * x
          END;
         
        BEGIN
           x := 1;
           WHILE x <= 10 DO
           BEGIN
             CALL square;
             ! squ;
             x := x + 1
           END
        END.
      """
      parse(s4).toEither should be ('right)      
    }

}
