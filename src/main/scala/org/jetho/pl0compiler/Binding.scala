
/** The runtime bindings used by the evaluators.*/


package org.jetho.pl0compiler


abstract class Binding
case class Number(num: Int) extends Binding
case class Closure(block: Block, env: Environment[Binding]) extends Binding
case object NullBinding extends Binding
