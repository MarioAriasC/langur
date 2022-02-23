package org.marioarias.langur.evaluator

import org.marioarias.langur.objects.{MBuiltinFunction, getBuiltinByName, builtins as fns}

/**
 * Created by IntelliJ IDEA.
 *
 * @author Mario Arias
 *         Date: 22/2/22
 *         Time: 8:48 AM
 */
object Builtins {
  private val builtins = Map(
    "len" -> fns.getBuiltinByName("len"),
    "push" -> fns.getBuiltinByName("push"),
    "first" -> fns.getBuiltinByName("first"),
    "last" -> fns.getBuiltinByName("last"),
    "rest" -> fns.getBuiltinByName("rest")
  )

  def apply(name: String): Option[MBuiltinFunction] = builtins.get(name).flatten
}
