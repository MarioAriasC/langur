package org.marioarias.langur.evaluator

import org.marioarias.langur.objects.MObject

import scala.collection.mutable

/** Created by IntelliJ IDEA.
  *
  * @author
  *   Mario Arias Date: 19/2/22 Time: 5:22 PM
  */
class Environment private (
    val store: mutable.HashMap[String, MObject],
    val outer: Option[Environment]
) {
  def put(name: String, value: MObject): MObject = {
    this(name) = value
    value
  }

  def update(name: String, value: MObject): Unit = {
    store(name) = value
  }

  def apply(name: String): Option[MObject] = {
    val obj = store.get(name)
    if (obj.isEmpty && outer.isDefined) {
      return outer.get(name)
    }
    obj
  }
}

object Environment {
  def newEnvironment(): Environment = Environment(mutable.HashMap.empty, None)

  def newEnclosedEnvironment(outer: Environment): Environment =
    Environment(mutable.HashMap.empty, Some(outer))
}
