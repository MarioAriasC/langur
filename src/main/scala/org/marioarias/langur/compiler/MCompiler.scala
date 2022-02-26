package org.marioarias.langur.compiler

import org.marioarias.langur.ast.Node
import org.marioarias.langur.code.Instructions
import org.marioarias.langur.objects.MObject

/**
 * Created by IntelliJ IDEA.
 *
 * @author Mario Arias
 *         Date: 26/2/22
 *         Time: 2:01 PM
 */
class MCompiler {
  def compile(node: Node): Unit = {

  }
  
  def bytecode:Bytecode = ???
}

class Bytecode(val instructions: Instructions, val constants: List[MObject]) {

  def canEqual(other: Any): Boolean = other.isInstanceOf[Bytecode]

  override def equals(other: Any): Boolean = other match {
    case that: Bytecode =>
      (that canEqual this) &&
        instructions == that.instructions &&
        constants == that.constants
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(instructions, constants)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}