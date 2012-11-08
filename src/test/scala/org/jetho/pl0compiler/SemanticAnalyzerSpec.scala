

package org.jetho.pl0compiler


import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import scalaz._
import Scalaz._


class SemanticAnalyzerSpec extends FlatSpec with ShouldMatchers {

  def analyze(ast: AST) = Semant.analyze(ast)

  def parseAndAnalyze(input: String) =
    PL0Parser.parse(input) >>= (Semant.analyze(_))



  "The Semantic Analyzer" should "reject multiple declarations of the same identifier in the same scope" in {
    
    analyze(Block(List(),List(VarDecl("a"), VarDecl("a")),List(),None)) should equal ("Multiple Definition of a".left)

    analyze(Block(List(ConstDecl("a",1), ConstDecl("a",2)),List(),List(),None)) should equal ("Multiple Definition of a".left)

    analyze(Block(List(),List(),List(ProcDecl("a",Block(List(),List(),List(),Some(SeqStmt(List())))), 
      ProcDecl("a",Block(List(),List(),List(),Some(SeqStmt(List()))))),None)) should equal ("Multiple Definition of a".left)

    analyze(Block(List(ConstDecl("a",1)),List(VarDecl("a")),List(),None)) should equal ("Multiple Definition of a".left) 
  
    analyze(Block(List(ConstDecl("a",1), ConstDecl("b",2)),List(),List(ProcDecl("a",Block(List(),List(),List(),
      Some(PrintStmt(IntLiteral(1)))))),None)) should equal ("Multiple Definition of a".left) 

    analyze(Block(List(ConstDecl("a",1), ConstDecl("b",2)),List(VarDecl("b")),List(ProcDecl("a",Block(List(),List(),List(),
      Some(PrintStmt(IntLiteral(1)))))),None)) should equal ("Multiple Definition of b\nMultiple Definition of a".left) 

  }

  it should "allow declarations of the same identifier in different lexical scopes" in {
    
    analyze(Block(List(ConstDecl("a",1)),List(),List(ProcDecl("b",Block(List(ConstDecl("a",2)),List(),List(),
      Some(PrintStmt(IntLiteral(1)))))),None)).toEither should be ('right)
  }
  
  it should "reject the assignment to constants" in {

    analyze(Block(List(ConstDecl("a",1)),List(),List(),Some(AssignStmt("a",IntLiteral(2))))) should equal ("Illegal Assignment to Constant a!".left)
  }

  it should "reject the use of undeclared constants, variables and procedures" in {

    analyze(Block(List(),List(),List(),Some(PrintStmt(Ident("a"))))) should equal ("Not a valid Variable or Constant: a!".left)

    analyze(Block(List(),List(),List(ProcDecl("a",Block(List(),List(),List(),Some(PrintStmt(IntLiteral(1)))))),
      Some(AssignStmt("a",IntLiteral(1))))) should equal ("Undefined Variable: a!".left)

    analyze(Block(List(),List(VarDecl("a")),List(),Some(CallStmt("a")))) should equal ("Undefined Procedure a!".left)
  }

  it should "accept semantically valid programs" in {
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
    parseAndAnalyze(s1).toEither should be ('right)

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
      parseAndAnalyze(s2).toEither should be ('right)

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
      parseAndAnalyze(s3).toEither should be ('right)

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
      parseAndAnalyze(s4).toEither should be ('right)      
    }

}
