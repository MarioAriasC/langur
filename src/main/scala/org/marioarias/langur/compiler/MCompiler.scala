package org.marioarias.langur.compiler

import org.marioarias.langur.ast.*
import org.marioarias.langur.code.*
import org.marioarias.langur.objects.{MInteger, MObject}

import scala.collection.mutable

/**
 * Created by IntelliJ IDEA.
 *
 * @author Mario Arias
 *         Date: 26/2/22
 *         Time: 2:01 PM
 */
class MCompiler {
  private val constants = mutable.ListBuffer.empty[MObject]
  private val scopes = mutable.ListBuffer(CompilationScope())
  var scopeIndex = 0

  def compile(node: Node): Unit = {
    node match {
      case p: Program => p.statements.foreach(compile)
      case es: ExpressionStatement =>
        compile(es.expression.get)
        emit(OpPop)
      case ie: InfixExpression =>
        if (ie.operator == "<") {
          compile(ie.right.get)
          compile(ie.left.get)
          emit(OpGreaterThan)
        } else {
          compile(ie.left.get)
          compile(ie.right.get)
          ie.operator match {
            case "+" => emit(OpAdd)
            case "-" => emit(OpSub)
            case "*" => emit(OpMul)
            case "/" => emit(OpDiv)
            case ">" => emit(OpGreaterThan)
            case "==" => emit(OpEqual)
            case "!=" => emit(OpNotEqual)
            case operator: _ => throw MCompilerException(s"unknown operator $operator")
          }
        }
      case il: IntegerLiteral => emit(OpConstant, addConstant(MInteger(il.value)))
      case pe: PrefixExpression =>
        compile(pe.right.get)
        pe.operator match {
          case "!" => emit(OpBang)
          case "-" => emit(OpMinus)
          case operator: _ => throw MCompilerException(s"unknown operator $operator")
        }
    }
  }

  def emit(op: Opcode, operands: Int*): Int = {
    val ins = make(op, operands *)
    val pos = addInstruction(ins)
    setLastInstruction(op, pos)
    pos
  }

  private def setLastInstruction(op: Opcode, position: Int): Unit = {
    val scope = currentScope
    val previous = scope.lastInstruction
    val last = EmittedInstruction(op, position)
    scope.previousInstruction = previous
    scope.lastInstruction = last
  }

  private def addInstruction(ins: Instructions): Int = {
    val pos = currentInstructions.length
    currentScope.instructions = currentInstructions ++ ins
    pos
  }

  private def addConstant(obj: MObject): Int = {
    constants += obj
    constants.size - 1
  }

  def bytecode: Bytecode = Bytecode(currentInstructions, constants.toList)

  def currentScope: CompilationScope = scopes(scopeIndex)

  private def currentInstructions = currentScope.instructions
}

class Bytecode(val instructions: Instructions, val constants: List[MObject]) {

  def canEqual(other: Any): Boolean = other.isInstanceOf[Bytecode]

  override def equals(other: Any): Boolean = other match {
    case that: Bytecode =>
      (that canEqual this) &&
        (instructions sameElements that.instructions) &&
        constants == that.constants
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(instructions, constants)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}

case class EmittedInstruction(var op: Opcode = 0.u, position: Int = 0)

class CompilationScope(var instructions: Instructions = Array[UB](),
                       var lastInstruction: EmittedInstruction = EmittedInstruction(),
                       var previousInstruction: EmittedInstruction = EmittedInstruction()) {

  def canEqual(other: Any): Boolean = other.isInstanceOf[CompilationScope]

  override def equals(other: Any): Boolean = other match {
    case that: CompilationScope =>
      (that canEqual this) &&
        instructions == that.instructions &&
        lastInstruction == that.lastInstruction &&
        previousInstruction == that.previousInstruction
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(instructions, lastInstruction, previousInstruction)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}

class MCompilerException(message: String) extends Exception(message)

