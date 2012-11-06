
/** The runtime data types used by the compilation module.*/


package org.jetho.pl0compiler


abstract class RuntimeEntity
case class Constant(value: Int) extends RuntimeEntity
case class Variable(address: EntityAddress) extends RuntimeEntity 
case class Proc(address: Option[EntityAddress]) extends RuntimeEntity
case class PrimitiveRoutine(displacement: Int) extends RuntimeEntity

case class EntityAddress(level: Int, displacement: Int)

