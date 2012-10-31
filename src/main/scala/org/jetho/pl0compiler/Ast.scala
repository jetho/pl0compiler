
/** The data types for the AST.*/


package org.jetho.pl0compiler


abstract class AST

case class Block(constDecls: List[ConstDecl], varDecls: List[VarDecl], procDecls: List[ProcDecl], statement: Option[Statement]) extends AST

abstract class Declaration(val ident: String) extends AST
case class ConstDecl(override val ident: String, num: Int) extends Declaration(ident)
case class VarDecl(override val ident: String) extends Declaration(ident)
case class ProcDecl(override val ident: String, block: Block) extends Declaration(ident)

abstract class Statement extends AST
case class AssignStmt(ident: String, expr: Expr) extends Statement
case class CallStmt(ident: String) extends Statement
case class SeqStmt(stmts: List[Statement]) extends Statement
case class IfStmt(condition: Condition, stmt: Option[Statement]) extends Statement
case class WhileStmt(condition: Condition, stmt: Option[Statement]) extends Statement
case class PrintStmt(expr: Expr) extends Statement

abstract class Condition extends AST
case class OddCondition(expr: Expr) extends Condition
case class BinaryCondition(op: String, leftExpr: Expr, rightExpr: Expr) extends Condition

abstract class Expr extends AST
case class BinOp(op: String, left: Expr, right: Expr) extends Expr
case class IntLiteral(num: Int) extends Expr
case class Ident(name: String) extends Expr
