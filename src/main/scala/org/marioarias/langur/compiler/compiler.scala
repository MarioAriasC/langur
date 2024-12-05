package org.marioarias.langur.compiler

import org.marioarias.langur.ast.*
import org.marioarias.langur.code.*
import org.marioarias.langur.objects.*
import org.marioarias.langur.utils.Utils.also

import scala.collection.mutable

/**
 * Created by IntelliJ IDEA.
 *
 * @author Mario Arias
 *         Date: 26/2/22
 *         Time: 2:01 PM
 */
class MCompiler(
                 private val constants: mutable.ListBuffer[MObject] = mutable.ListBuffer.empty[MObject],
                 var symbolTable: SymbolTable = SymbolTable()
               ) {
  builtins.zipWithIndex.foreach { case ((name, _), i) =>
    symbolTable.defineBuiltin(i, name)
  }
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
            case operator => throw MCompilerException(s"unknown operator $operator")
          }
        }
      case il: IntegerLiteral => emit(OpConstant, addConstant(MInteger(il.value)))
      case pe: PrefixExpression =>
        compile(pe.right.get)
        pe.operator match {
          case "!" => emit(OpBang)
          case "-" => emit(OpMinus)
          case operator => throw MCompilerException(s"unknown operator $operator")
        }
      case bl: BooleanLiteral => if (bl.value) emit(OpTrue) else emit(OpFalse)
      case ie: IfExpression =>
        compile(ie.condition.get)
        val jumpNotTruthyPos = emit(OpJumpNotTruthy, 9999)
        compile(ie.consequence.get)
        if (isLastInstructionPop) {
          removeLastPop()
        }
        val jumpPos = emit(OpJump, 9999)
        val afterConsequencePos = currentInstructions.length
        changeOperand(jumpNotTruthyPos, afterConsequencePos)
        if (ie.alternative.isEmpty) {
          emit(OpNull)
        } else {
          compile(ie.alternative.get)
          if (isLastInstructionPop) {
            removeLastPop()
          }
        }
        val afterAlternativePos = currentInstructions.length
        changeOperand(jumpPos, afterAlternativePos)
      case bs: BlockStatement => bs.statements.foreach(_.foreach(statement => compile(statement.get)))
      case ls: LetStatement =>
        val symbol = symbolTable.define(ls.name.value)
        compile(ls.value.get)
        if (symbol.scope == SymbolScope.GLOBAL) {
          emit(OpSetGlobal, symbol.index)
        } else {
          emit(OpSetLocal, symbol.index)
        }
      case id: Identifier =>
        val symbol = symbolTable.resolve(id.value)
        loadSymbol(symbol)
      case sl: StringLiteral =>
        val str = MString(sl.value)
        emit(OpConstant, addConstant(str))
      case al: ArrayLiteral =>
        al.elements.foreach(_.foreach(element => compile(element.get)))
        emit(OpArray, al.elements.get.length)
      case hl: HashLiteral =>
        val keys = hl.pairs.keys.toArray.sortBy(_.toString)
        keys.foreach { key =>
          compile(key)
          compile(hl.pairs(key))
        }
        emit(OpHash, hl.pairs.size * 2)
      case ie: IndexExpression =>
        compile(ie.left.get)
        compile(ie.index.get)
        emit(OpIndex)
      case fl: FunctionLiteral =>
        enterScope()
        if (fl.name.nonEmpty) {
          symbolTable.defineFunctionName(fl.name)
        }
        fl.parameters.foreach(_.foreach(parameter => symbolTable.define(parameter.value)))
        compile(fl.body.get)
        if (isLastInstructionPop) {
          replaceLastPopWithReturn()
        }
        if (!lastInstructionIs(OpReturnValue)) {
          emit(OpReturn)
        }

        val freeSymbols = symbolTable.freeSymbols
        val numLocals = symbolTable.numDefinitions
        val instructions = leaveScope()
        freeSymbols.foreach(loadSymbol)
        val compiledFn = MCompiledFunction(instructions, numLocals, fl.parameters.get.length)
        emit(OpClosure, addConstant(compiledFn), freeSymbols.length)
      case rs: ReturnStatement =>
        compile(rs.returnValue.get)
        emit(OpReturnValue)
      case ce: CallExpression =>
        compile(ce.function.get)
        ce.arguments.foreach(_.foreach(arg => compile(arg.get)))
        emit(OpCall, ce.arguments.get.length)
    }
  }

  def emit(op: Opcode, operands: Int*): Int = {
    val ins = make(op, operands *)
    val pos = addInstruction(ins)
    setLastInstruction(op, pos)
    pos
  }

  def enterScope(): Unit = {
    scopes.addOne(CompilationScope())
    symbolTable = SymbolTable(outer = Some(symbolTable))
    scopeIndex = scopeIndex + 1
  }

  def leaveScope(): Instructions = {
    val instructions = currentInstructions
    scopes.remove(scopes.length - 1)
    scopeIndex = scopeIndex - 1
    symbolTable = symbolTable.outer.get
    instructions
  }

  private def loadSymbol(symbol: Symbol): Unit = {
    val opcode = symbol.scope match {
      case SymbolScope.GLOBAL => OpGetGlobal
      case SymbolScope.LOCAL => OpGetLocal
      case SymbolScope.BUILTIN => OpGetBuiltin
      case SymbolScope.FREE => OpGetFree
      case SymbolScope.FUNCTION => OpCurrentClosure
    }

    if (opcode != OpCurrentClosure) {
      emit(opcode, symbol.index)
    } else {
      emit(opcode)
    }
  }

  private def changeOperand(opPos: Int, operand: Int): Unit = {
    val op = currentInstructions(opPos)
    val newInstruction = make(op, operand)
    replaceInstruction(opPos, newInstruction)
  }

  private def replaceLastPopWithReturn(): Unit = {
    val lasPos = currentScope.lastInstruction.position
    replaceInstruction(lasPos, make(OpReturnValue))
    currentScope.lastInstruction.op = OpReturnValue
  }

  private def replaceInstruction(pos: Int, newInstruction: Instructions): Unit = {
    for (i <- newInstruction.indices) {
      currentInstructions(pos + i) = newInstruction(i)
    }
  }

  private def isLastInstructionPop: Boolean = lastInstructionIs(OpPop)

  private def lastInstructionIs(op: Opcode) = currentScope.lastInstruction.op == op

  private def removeLastPop(): Unit = {
    val scope = currentScope
    val last = scope.lastInstruction
    val previous = scope.previousInstruction

    val old = currentInstructions
    val newInstruction = old.onset(last.position)
    scope.instructions = newInstruction
    scope.lastInstruction = previous
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
      that.canEqual(this) &&
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
      that.canEqual(this) &&
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

