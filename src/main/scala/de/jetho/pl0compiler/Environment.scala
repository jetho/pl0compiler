
/** The environment structure for implementing lexical scoping. Used by the typechecker and interpreter.*/


package de.jetho.pl0compiler

import scala.collection.mutable.Map


/** The Environment stores binding ids and their values.
    Holds a reference to its parent Environment (containing scope).*/
class Environment[T](ids: Map[String, T], parent: Option[Environment[T]]) {
    
  /** create a nested environment by creating a new one with the current as parent.*/
  def extend(bindings: List[(String, T)]) = new Environment(Map(bindings : _*), Some(this))
  
  /** lookup the binding for id in the current lexical scope or in the containing scope.*/
  def resolve(id: String): Option[T] = 
    if (ids contains id) 
      Some(ids(id))
    else 
      parent.flatMap(_.resolve(id)) 

  /** Update a binding in the current or containing scope.*/
  def update(id: String, value: T) {
    if (ids contains id) 
      ids(id) = value
    else 
      parent.map(_.update(id, value))  
  }

}


/** The empty environment is used as initial environment when starting typechecking, interpreting or compiling.*/
object EmptyEnvironment {
  
  def apply[T] = new Environment(Map[String,T](), None)

}


