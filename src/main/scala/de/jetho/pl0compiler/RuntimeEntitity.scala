
package de.jetho.pl0compiler


abstract class RuntimeEntity
case class Constant(value: Int) extends RuntimeEntity
case class Variable(address: EntityAddress) extends RuntimeEntity 
case class Proc(address: Option[EntityAddress]) extends RuntimeEntity
case class PrimitiveRoutine(displacement: Int) extends RuntimeEntity

case class EntityAddress(level: Int, displacement: Int)

case class PatchLocation(displacement: Int, ident: String, env: Environment[RuntimeEntity], frame: Frame)


