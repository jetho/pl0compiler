
package de.jetho.pl0compiler

import scala.collection.mutable.Map


class Environment[T](ids: Map[String, T], parent: Option[Environment[T]]) {
    
  def extend(bindings: List[(String, T)]) = new Environment(Map(bindings : _*), Some(this))
  
  def resolve(id: String): Option[T] = 
    if (ids contains id) 
      Some(ids(id))
    else 
      parent.flatMap(_.resolve(id)) 

  def update(id: String, value: T) {
    if (ids contains id) 
      ids(id) = value
    else 
      parent.map(_.update(id, value))  
  }

}


object EmptyEnvironment {
  
  def apply[T] = new Environment(Map[String,T](), None)

}


