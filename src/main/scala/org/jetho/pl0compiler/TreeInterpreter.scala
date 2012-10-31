
/** Recursive AST based interpreter.*/


package org.jetho.pl0compiler


object TreeInterpreter {

  type EvalEnvironment = Environment[Binding]

  
  /** evaluate the AST starting out with an empty environment.*/
  def eval(ast: AST) = evalAst(ast, EmptyEnvironment[Binding])

  /** interpret the various language constructs recursively.*/
  def evalAst(ast: AST, env: EvalEnvironment) {
    ast match {
      
      /** interpret a block by creating a new lexical environment.*/
      case Block(constDecls, varDecls, procDecls, statement) => 
        statement.map { 
          stmt => {
            val constBindings = constDecls.map { case ConstDecl(id, num) => (id, Number(num)) }
            val varBindings = varDecls.map { varDecl => (varDecl.ident, Number(0)) }
            val procBindings = procDecls.map { procDecl => (procDecl.ident, NullBinding) }
            val extendedEnvironment = env.extend( constBindings ::: varBindings ::: procBindings )
            procDecls.foreach { case ProcDecl(id, block) => extendedEnvironment.update(id, Closure(block, extendedEnvironment)) }
            evalAst(stmt, extendedEnvironment)
          }
        }   

      case SeqStmt(stmts) => stmts.foreach { evalAst(_, env) }

      case CallStmt(ident) => env.resolve(ident).map { case Closure(block, closureEnv) => evalAst(block, closureEnv) }

      case AssignStmt(ident, expr) => env.update(ident, Number(evalExpression(expr, env)))

      case PrintStmt(expr) => println(evalExpression(expr, env))

      case WhileStmt(condition, stmt) => stmt.map { stmt => while(evalCondition(condition, env)) evalAst(stmt, env) }

      case IfStmt(condition, stmt) => stmt.map { stmt => if(evalCondition(condition, env)) evalAst(stmt, env) }
    }
  }

  /** evaluate a numeric expression.*/
  def evalExpression(expr: Expr, env: EvalEnvironment): Int = 
    expr match {
      case BinOp(op, left, right) => {
	val leftVal = evalExpression(left, env)
	val rightVal = evalExpression(right, env)
	op match { 
	  case "+" => leftVal + rightVal
	  case "-" => leftVal - rightVal
	  case "*" => leftVal * rightVal
	  case "/" => if(rightVal != 0) leftVal / rightVal else throw new RuntimeException("Illegal division by 0!")
	}
      }
      case IntLiteral(n) => n
      case Ident(name) => env.resolve(name).map{ case Number(n) => n }.getOrElse(sys.error("Illegal Number"))
    }

  /** evaluate a boolean condition.*/
  def evalCondition(condition: Condition, env: EvalEnvironment): Boolean =
    condition match {
      case OddCondition(expr) => (evalExpression(expr, env) % 2) == 1

      case BinaryCondition(op, leftExpr, rightExpr) => {
        val leftValue = evalExpression(leftExpr, env)
        val rightValue = evalExpression(rightExpr, env)
        op match {
          case "=" => leftValue == rightValue
          case "#" => leftValue != rightValue
          case "<" => leftValue < rightValue
          case "<=" => leftValue <= rightValue
          case ">" => leftValue > rightValue
          case ">=" => leftValue >= rightValue
        }
      }
    }      
 
}
