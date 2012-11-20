

package org.jetho.pl0compiler


import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import scalaz._
import Scalaz._


class CodeGeneratorSpec extends FlatSpec with ShouldMatchers {

  def parseAnalyzeEncode(input: String) =
    PL0Parser.parse(input) >>= (Semant.analyze(_)) >>= (CodeGenerator.encodeAst(_))


  "The Semantic Analyzer" should "reject more than 1024 instructions" in {
    // a print statement generates 2 instructions
    val s = "BEGIN" + List.fill(512)("!1;").mkString + " !1 END ."   
    parseAnalyzeEncode(s) should equal ("Can't process more than 1024 Instructions!".left)
  }

  it should "reject procedure nesting levels greater than 6" in {
    val s = """
      PROCEDURE proc1;
      VAR x;  
      PROCEDURE proc2;
        VAR y;    
        PROCEDURE proc3;
            PROCEDURE proc4;
                PROCEDURE proc5;
                    PROCEDURE proc6;
                        PROCEDURE proc7;
                            PROCEDURE proc8;
                              BEGIN
                                x := 5;
                                y := 6;
                              END;
                          BEGIN						
                          END;
                      BEGIN					
                      END;
                  BEGIN				
                  END;
              BEGIN			
              END;
          BEGIN       
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
    parseAnalyzeEncode(s) should equal ("Can't access data more than 6 levels out!".left)
  }      

  it should "encode valid programs" in {
    
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
    parseAnalyzeEncode(s1) should equal (
      List(Instruction(10,0,0,1,None,None,None), Instruction(12,0,0,28,None,None,None), 
      Instruction(10,0,0,2,None,None,None), Instruction(12,0,0,18,None,None,None), Instruction(0,9,1,3,None,None,None), 
      Instruction(0,4,1,0,None,None,None), Instruction(6,2,4,14,None,None,None), Instruction(14,0,0,17,None,None,None), 
      Instruction(0,9,1,4,None,None,None), Instruction(0,9,1,3,None,None,None), Instruction(6,2,4,10,None,None,None), 
      Instruction(4,9,1,4,None,None,None), Instruction(0,9,1,3,None,None,None), Instruction(3,0,0,1,None,None,None), 
      Instruction(6,2,4,8,None,None,None), Instruction(4,9,1,3,None,None,None), Instruction(6,0,9,4,None,None,None),
      Instruction(8,0,0,0,None,None,None), Instruction(3,0,0,1,None,None,None), Instruction(4,8,1,3,None,None,None), 
      Instruction(3,0,0,1,None,None,None), Instruction(4,8,1,4,None,None,None), Instruction(6,0,8,4,None,None,None), 
      Instruction(0,8,1,4,None,None,None), Instruction(6,2,4,26,None,None,None), Instruction(6,2,4,24,None,None,None), 
      Instruction(11,0,0,2,None,None,None), Instruction(8,0,0,0,None,None,None), Instruction(3,0,0,6,None,None,None), 
      Instruction(4,4,1,0,None,None,None), Instruction(6,0,4,2,None,None,None), Instruction(11,0,0,1,None,None,None), 
      Instruction(15,0,0,0,None,None,None)).right)


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
      parseAnalyzeEncode(s2) should equal (
        List(Instruction(12,0,0,14,None,None,None), Instruction(12,0,0,7,None,None,None), Instruction(12,0,0,5,None,None,None),
        Instruction(6,0,10,8,None,None,None), Instruction(8,0,0,0,None,None,None), Instruction(6,0,8,3,None,None,None), 
        Instruction(8,0,0,0,None,None,None), Instruction(12,0,0,12,None,None,None), Instruction(3,0,0,7,None,None,None), 
        Instruction(6,2,4,26,None,None,None), Instruction(6,2,4,24,None,None,None), Instruction(8,0,0,0,None,None,None), 
        Instruction(6,0,8,2,None,None,None), Instruction(8,0,0,0,None,None,None), Instruction(6,0,4,1,None,None,None),
        Instruction(15,0,0,0,None,None,None)).right)


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
      parseAnalyzeEncode(s3) should equal (
        List(Instruction(12,0,0,22,None,None,None), Instruction(10,0,0,1,None,None,None), Instruction(12,0,0,16,None,None,None),
         Instruction(10,0,0,1,None,None,None), Instruction(12,0,0,10,None,None,None), Instruction(3,0,0,5,None,None,None), 
         Instruction(4,10,1,3,None,None,None), Instruction(3,0,0,6,None,None,None),   Instruction(4,9,1,3,None,None,None), 
         Instruction(8,0,0,0,None,None,None), Instruction(6,0,8,5,None,None,None), Instruction(0,8,1,3,None,None,None), 
         Instruction(6,2,4,26,None,None,None), Instruction(6,2,4,24,None,None,None), Instruction(11,0,0,1,None,None,None), 
         Instruction(8,0,0,0,None,None,None), Instruction(6,0,8,3,None,None,None), Instruction(0,8,1,3,None,None,None), 
         Instruction(6,2,4,26,None,None,None), Instruction(6,2,4,24,None,None,None), Instruction(11,0,0,1,None,None,None), 
         Instruction(8,0,0,0,None,None,None), Instruction(6,0,4,1,None,None,None), Instruction(15,0,0,0,None,None,None)).right)
      

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
      parseAnalyzeEncode(s4) should equal (
        List(Instruction(10,0,0,2,None,None,None), Instruction(12,0,0,7,None,None,None), Instruction(0,4,1,0,None,None,None), 
        Instruction(0,4,1,0,None,None,None), Instruction(6,2,4,10,None,None,None), Instruction(4,4,1,1,None,None,None), 
        Instruction(8,0,0,0,None,None,None), Instruction(3,0,0,1,None,None,None), Instruction(4,4,1,0,None,None,None), 
        Instruction(12,0,0,18,None,None,None), Instruction(6,0,4,2,None,None,None), Instruction(0,4,1,1,None,None,None), 
        Instruction(6,2,4,26,None,None,None), Instruction(6,2,4,24,None,None,None), Instruction(0,4,1,0,None,None,None), 
        Instruction(3,0,0,1,None,None,None), Instruction(6,2,4,8,None,None,None), Instruction(4,4,1,0,None,None,None), 
        Instruction(0,4,1,0,None,None,None), Instruction(3,0,0,10,None,None,None), Instruction(6,2,4,14,None,None,None), 
        Instruction(14,0,1,10,None,None,None), Instruction(11,0,0,2,None,None,None), Instruction(15,0,0,0,None,None,None)).right)
      
    }
}
