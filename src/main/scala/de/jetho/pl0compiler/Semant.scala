
package de.jetho.pl0compiler


object Semant {

  type ErrorMessage = String
  type SemanticEnvironment = Environment[Declaration]

 
  def check(ast: AST): Option[AST] = 
    checkAst(ast, EmptyEnvironment[Declaration]) match {
      case Nil => Some(ast)
      case list => list.foreach{ Console.err.println(_) }
                   None
    } 
    
  def checkAstList(asts: List[AST], env: SemanticEnvironment): List[ErrorMessage] = asts.map { checkAst(_, env) }.flatten

  def checkAst(ast: AST, env: SemanticEnvironment): List[ErrorMessage] = 
    ast match {

      case Block(constDecls, varDecls, procDecls, statement) => {   
        val bindings = List(constDecls, varDecls, procDecls).flatten
        val declarationErrors = bindings.groupBy(_.ident).filter(_._2.length > 1).keys.map("Multiple Definition of Identifier " + _).toList
        val newLexicalEnvironment = env.extend( bindings.map {decl => (decl.ident, decl)} )
        val procErrors = checkAstList(procDecls, newLexicalEnvironment)
        val bodyErrors = statement.map(checkAst(_, newLexicalEnvironment)).getOrElse(Nil)
        declarationErrors ::: procErrors ::: bodyErrors
      }

      case ProcDecl(ident, block) => checkAst(block, env) 

      case AssignStmt(ident, expr) => checkForVariable(ident, env).toList ::: checkAst(expr, env)

      case CallStmt(ident) => checkForProcedure(ident, env).toList

      case PrintStmt(expr) => checkAst(expr, env)

      case SeqStmt(stmts) => checkAstList(stmts, env)

      case IfStmt(condition, stmt) => 
        checkAst(condition, env) ::: stmt.map(checkAst(_, env)).getOrElse(Nil)

      case WhileStmt(condition, stmt) => 
        checkAst(condition, env) ::: stmt.map(checkAst(_, env)).getOrElse(Nil)
      
      case OddCondition(expr) => checkAst(expr, env)

      case BinaryCondition(_, leftExpr, rightExpr) => 
        checkAst(leftExpr, env) ::: checkAst(rightExpr, env)

      case BinOp(_, left, right) => checkAstList(List(left, right), env) 
      
      case Ident(name) => checkForValue(name, env).toList

      case IntLiteral(_) => Nil      
           
      case _ => throw new RuntimeException("Unknown AST-Node: " + ast)
    }


  def checkForValue(ident: String, env: SemanticEnvironment) = env.resolve(ident) match {
    case Some(_: VarDecl) => None
    case Some(_: ConstDecl) => None
    case _ => Some("Not a valid Variable or Constant: " + ident + "!")
  }
   
  def checkForVariable(ident: String, env: SemanticEnvironment) = env.resolve(ident) match {
    case Some(_: VarDecl) => None
    case Some(_: ConstDecl) => Some("Illegal Assignment to Constant " + ident + "!")
    case _ => Some("Undefined Variable " + ident + "!")
  }

  def checkForProcedure(ident: String, env: SemanticEnvironment) = env.resolve(ident) match {
    case Some(_: ProcDecl) => None   
    case _ => Some("Undefined Procedure " + ident + "!")
  }  
 
}
