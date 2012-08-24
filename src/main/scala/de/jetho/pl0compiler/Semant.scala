
/** Semantic Analyzer for the AST.*/


package de.jetho.pl0compiler

import scalaz._
import Scalaz._


object Semant {

  type SemanticEnvironment = Environment[Declaration]

 
  /** check the AST for semantic errors.*/
  def check(ast: AST): ValidationNEL[String, AST] = checkAst(ast, EmptyEnvironment[Declaration])


  /** helper function for analyzing a list of AST nodes.*/
  def checkAstList(asts: List[AST], env: SemanticEnvironment) = 
    asts.map(checkAst(_, env)).sequence[({type l[a]=ValidationNEL[String, a]})#l, AST]    


  /** analyze the various language constructs.*/
  def checkAst(ast: AST, env: SemanticEnvironment): ValidationNEL[String, AST] = 
    ast match {
     
      case bl@ Block(constDecls, varDecls, procDecls, stmt) =>   
        // building a new lexical environment
        val bindings = constDecls ::: varDecls ::: procDecls
        val newLexicalEnvironment = env.extend( bindings.map {decl => (decl.ident, decl)} )

        // using an applicative functor for accumulating errors
        (checkUniqueness(bindings) |@|
         checkAstList(procDecls, newLexicalEnvironment) |@|
         stmt.map(checkAst(_, env)).getOrElse(stmt.successNel[String])) {
           (_, _, _) => bl
         }

      case ProcDecl(ident, block) => checkAst(block, env) 

      case as@ AssignStmt(ident, expr) => 
        (checkForVariable(ident, env).liftFailNel |@| checkAst(expr, env)) { (_, _) => as }

      case CallStmt(ident) => checkForProcedure(ident, env).liftFailNel

      case PrintStmt(expr) => checkAst(expr, env)

      case sq@ SeqStmt(stmts) => checkAstList(stmts, env).map(_ => sq) 

      case is@ IfStmt(condition, stmt) =>  
        (checkAst(condition, env) |@| stmt.map(checkAst(_, env)).getOrElse(stmt.successNel[String])) {
          (_, _) => is
        }          

      case ws@ WhileStmt(condition, stmt) => 
        (checkAst(condition, env) |@| stmt.map(checkAst(_, env)).getOrElse(stmt.successNel[String])) {
          (_, _) => ws
        }          
      
      case OddCondition(expr) => checkAst(expr, env)

      case bc@ BinaryCondition(_, left, right) =>  checkAstList(List(left, right), env).map(_ => bc) 

      case bo@ BinOp(_, left, right) => checkAstList(List(left, right), env).map(_ => bo) 
      
      case Ident(name) => checkForValue(name, env).liftFailNel

      case il@ IntLiteral(_) => il.successNel[String]
           
      case _ => ("Unknown AST-Node: " + ast).failNel[AST]
    } 


  /** analyze if there are multiple definitions of the same identifier.*/
  def checkUniqueness(decls: List[Declaration]): ValidationNEL[String, List[Declaration]] = {
    def isUnique(declGroup: (String, List[Declaration])) = 
      validation((declGroup._2.length > 1) either ("Multiple Definition of " + id) or declGroup._2.head)
   
    decls.groupBy(_.ident).map(isUnique(_).liftFailNel).toList.sequence[({type l[a]=ValidationNEL[String, a]})#l, Declaration]  
  }              
    
  
  /** lookup the identifier and apply the given constraint.*/
  def checkIdentifier(ident: String, env: SemanticEnvironment) (constraint: PartialFunction[AST, Validation[String, AST]]) =
    env.resolve(ident) flatMap constraint.lift


  def checkForValue(ident: String, env: SemanticEnvironment) =
    checkIdentifier(ident, env) {
      case v: VarDecl => v.success
      case c: ConstDecl => c.success
    }.getOrElse(("Not a valid Variable or Constant: " + ident + "!").fail)


  def checkForVariable(ident: String, env: SemanticEnvironment) =
    checkIdentifier(ident, env) {
      case v: VarDecl => v.success
      case c: ConstDecl => ("Illegal Assignment to Constant " + ident + "!").fail
    }.getOrElse(("Undefined Variable: " + ident + "!").fail)


  def checkForProcedure(ident: String, env: SemanticEnvironment) =
    checkIdentifier(ident, env) {
      case p: ProcDecl => p.success     
    }.getOrElse(("Undefined Procedure " + ident + "!").fail)

}
