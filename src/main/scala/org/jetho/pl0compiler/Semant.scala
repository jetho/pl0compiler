/** Semantic Analyzer for the AST.*/


package org.jetho.pl0compiler

import scalaz._
import Scalaz._


object Semant {

  type SemanticEnvironment = Environment[Declaration]

 
  /** check the AST for semantic errors.*/
  def analyze(ast: AST): \/[List[String], AST] = 
    analyzeAst(ast, EmptyEnvironment[Declaration]).bimap(_.toList, identity).disjunction


  /** helper function for analyzing a list of AST nodes.*/
  def analyzeAsts(asts: List[AST], env: SemanticEnvironment) = 
    asts.map(analyzeAst(_, env)).sequence[({type l[a]=ValidationNEL[String, a]})#l, AST]    


  /** analyze the various language constructs.*/
  def analyzeAst(ast: AST, env: SemanticEnvironment): ValidationNEL[String, AST] = 
    ast match {
     
      case bl@ Block(constDecls, varDecls, procDecls, stmt) =>   
        // building a new lexical environment
        val bindings = constDecls ::: varDecls ::: procDecls
        val extendedEnv = env.extend( bindings.map {decl => (decl.ident, decl)} )

        // using an applicative functor for accumulating errors
        (checkUniqueness(bindings)            |@|
         analyzeAsts(procDecls, extendedEnv) |@|
         stmt.map(analyzeAst(_, extendedEnv)).getOrElse(stmt.successNel[String])) {
           (_, _, _) => bl
         }

      case ProcDecl(ident, block) => analyzeAst(block, env) 

      case as@ AssignStmt(ident, expr) => 
        (checkForVariable(ident, env).toValidationNEL |@| analyzeAst(expr, env)) { (_, _) => as }

      case CallStmt(ident) => checkForProcedure(ident, env).toValidationNEL 

      case PrintStmt(expr) => analyzeAst(expr, env)

      case sq@ SeqStmt(stmts) => analyzeAsts(stmts, env).map(_ => sq) 

      case is@ IfStmt(condition, stmt) =>  
        (analyzeAst(condition, env) |@| stmt.map(analyzeAst(_, env)).getOrElse(stmt.successNel[String])) {
          (_, _) => is
        }          

      case ws@ WhileStmt(condition, stmt) => 
        (analyzeAst(condition, env) |@| stmt.map(analyzeAst(_, env)).getOrElse(stmt.successNel[String])) {
          (_, _) => ws
        }          
      
      case OddCondition(expr) => analyzeAst(expr, env)

      case bc@ BinaryCondition(_, left, right) =>  analyzeAsts(List(left, right), env).map(_ => bc) 

      case bo@ BinOp(_, left, right) => analyzeAsts(List(left, right), env).map(_ => bo) 
      
      case Ident(name) => checkForValue(name, env).toValidationNEL

      case il@ IntLiteral(_) => il.successNel[String]
           
      case _ => ("Unknown AST-Node: " + ast).failureNel[AST]
    } 


  /** analyze if there are multiple definitions of the same identifier.*/
  def checkUniqueness(decls: List[Declaration]): ValidationNEL[String, List[Declaration]] = {
    def isUnique(declGroup: (String, List[Declaration])) = 
      ((declGroup._2.length > 1) either ("Multiple Definition of " + declGroup._1) or declGroup._2.head).validation
   
    val validatedDecls = decls.groupBy(_.ident).map(isUnique(_).toValidationNEL).toList
    validatedDecls.sequence[({type l[a]=ValidationNEL[String, a]})#l, Declaration]  
  }              
    
  
  /** lookup the identifier and apply the given constraint.*/
  def checkIdentifier(ident: String, env: SemanticEnvironment) (constraint: PartialFunction[AST, Validation[String, AST]]) =
    env.resolve(ident) flatMap constraint.lift


  def checkForValue(ident: String, env: SemanticEnvironment) =
    checkIdentifier(ident, env) {
      case v: VarDecl => v.success
      case c: ConstDecl => c.success
    }.getOrElse(("Not a valid Variable or Constant: " + ident + "!").failure)


  def checkForVariable(ident: String, env: SemanticEnvironment) = 
    checkIdentifier(ident, env) {
      case v: VarDecl => v.success
      case c: ConstDecl => ("Illegal Assignment to Constant " + ident + "!").failure
    }.getOrElse(("Undefined Variable: " + ident + "!").failure)


  def checkForProcedure(ident: String, env: SemanticEnvironment) =
    checkIdentifier(ident, env) {
      case p: ProcDecl => p.success     
    }.getOrElse(("Undefined Procedure " + ident + "!").failure)

}
