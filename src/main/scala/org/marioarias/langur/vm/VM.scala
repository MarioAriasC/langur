package org.marioarias.langur.vm

import org.marioarias.langur.code.*
import org.marioarias.langur.compiler.Bytecode
import org.marioarias.langur.objects.*
import org.marioarias.langur.utils.Utils.also
import org.marioarias.langur.vm.VM.*

import scala.collection.mutable

/**
 * Created by IntelliJ IDEA.
 *
 * @author Mario Arias
 *         Date: 2/3/22
 *         Time: 8:21 AM
 */
class VM(bytecode: Bytecode, private val globals: mutable.ListBuffer[MObject] = mutable.ListBuffer.empty) {
  private val constants = bytecode.constants
  private val stack = Array.fill[Option[MObject]](STACK_SIZE) {
    None
  }
  private val frames = Array.fill[Option[Frame]](MAX_FRAME_SIZE) {
    None
  }
  private var sp = 0
  private var frameIndex = 1
  frames(0) = Some(Frame(MClosure(MCompiledFunction(bytecode.instructions)), 0))

  private def currentFrame: Frame = frames(frameIndex - 1).get

  def run(): Unit = {
    var ip: Int = -1
    var ins: Instructions = null
    var op: Opcode = -1.u
    while (currentFrame.ip < currentFrame.instructions.length - 1) {
      currentFrame.ip = currentFrame.ip + 1
      ip = currentFrame.ip
      ins = currentFrame.instructions
      op = ins(ip)
      op match {
        case OpConstant =>
          val constIndex = ins.readInt(ip + 1)
          currentFrame.ip += 2
          push(constants(constIndex))
        case OpPop => pop()
        case OpAdd => executeBinaryOperation(op)
        case OpSub => executeBinaryOperation(op)
        case OpMul => executeBinaryOperation(op)
        case OpDiv => executeBinaryOperation(op)
        case OpMinus => executeMinusOperator()
        case OpTrue => push(True)
        case OpFalse => push(False)
        case OpGreaterThan => executeComparison(op)
        case OpEqual => executeComparison(op)
        case OpNotEqual => executeComparison(op)
        case OpBang =>
          pop() match {
            case Some(True) => push(False)
            case Some(False) => push(True)
            case Some(Null) => push(True)
            case _ => push(False)
          }
        case OpJumpNotTruthy =>
          val pos = ins.readInt(ip + 1)
          currentFrame.ip += 2
          val condition = pop()
          if (!condition.isTruthy) {
            currentFrame.ip = pos - 1
          }
        case OpNull => push(Null)
        case OpJump =>
          val pos = ins.readInt(ip + 1)
          currentFrame.ip = pos - 1
        case OpSetGlobal =>
          val globalIndex = ins.readInt(ip + 1)
          currentFrame.ip += 2
          globals(globalIndex) = pop()
        case OpGetGlobal =>
          val globalIndex = ins.readInt(ip + 1)
          currentFrame.ip += 2
          push(globals(globalIndex))
        case OpArray => buildAndPush(ins, ip, buildArray)
        case OpHash => buildAndPush(ins, ip, buildHash)
        case OpIndex =>
          val index = pop()
          val left = pop()
          executeIndexExpression(left.get, index.get)
        case OpClosure =>
          val constIndex = ins.readInt(ip + 1)
          val numFree = ins.readByte(ip + 3)
          currentFrame.ip += 3
          pushClosure(constIndex, numFree.toInt)
        case OpCall =>
          val numArgs = ins.readByte(ip + 1)
          currentFrame.ip += 1
          executeCall(numArgs.toInt)
        case OpReturnValue =>
          val returnValue = pop()
          val frame = popFrame()
          sp = frame.basePointer - 1
          push(returnValue.get)
        case OpReturn =>
          val frame = popFrame()
          sp = frame.basePointer - 1
          push(Null)
        case OpSetLocal =>
          val localIndex = ins.readByte(ip + 1)
          currentFrame.ip += 1
          val frame = currentFrame
          stack(frame.basePointer + localIndex.toInt) = pop()
        case OpGetLocal =>
          val localIndex = ins.readByte(ip + 1)
          currentFrame.ip += 1
          val frame = currentFrame
          push(stack(frame.basePointer + localIndex.toInt).get)
        case OpGetBuiltin =>
          val builtinIndex = ins.readByte(ip + 1)
          currentFrame.ip += 1
          val builtin = builtins(builtinIndex.toInt)
          push(builtin._2)
        case OpGetFree =>
          val freeIndex = ins.readByte(ip + 1)
          currentFrame.ip += 1
          val currentClosure = currentFrame.cl
          push(currentClosure.free(freeIndex.toInt))
        case OpCurrentClosure =>
          val currentClosure = currentFrame.cl
          push(currentClosure)
      }
    }
  }

  private def buildAndPush(ins: Instructions, ip: Int, build: (Int, Int) => MObject): Unit = {
    val numElements = ins.readInt(ip + 1)
    currentFrame.ip += 2
    val col = build(sp - numElements, sp)
    sp -= numElements
    push(col)
  }

  private def buildHash(start: Int, end: Int): MObject = {
    val hashedPairs = mutable.Map.empty[HashKey, HashPair]
    for (i <- start until end by 2) {
      val key = stack(i)
      val value = stack(i + 1)
      if (key.isDefined && value.isDefined) {
        val pair = HashPair(key.get, value.get)
        key match {
          case Some(h: MHashable[?]) => hashedPairs(h.hashKey()) = pair
          case _ => throw VMException(s"unusable as hash key: ${key.typeDesc()}")
        }
      }
    }
    MHash(hashedPairs.toMap)
  }

  private def buildArray(start: Int, end: Int): MObject = {
    val elements = Array.fill[Option[MObject]](end - start) {
      None
    }
    var i = start
    while (i < end) {
      elements(i - start) = stack(i)
      i += 1
    }
    MArray(elements.toList)
  }

  private def executeCall(numArgs: Int): Unit = {
    stack(sp - 1 - numArgs) match {
      case Some(c: MClosure) => callClosure(c, numArgs)
      case Some(b: MBuiltinFunction) => callBuiltin(b, numArgs)
      case _ => throw VMException("calling non-function or non-builtin-in")
    }
  }

  private def callBuiltin(function: MBuiltinFunction, numArgs: Int): Unit = {
    val args = stack.slice(sp - numArgs, sp)
    val result = function.fn(args.toList)
    sp = sp - numArgs - 1
    result match {
      case Some(r: MObject) => push(r)
      case None => push(Null)
    }
  }

  private def callClosure(cl: MClosure, numArgs: Int): Unit = {
    if (cl.fn.numParameters != numArgs) {
      throw VMException(s"wrong number of arguments: want=${cl.fn.numParameters}, got=$numArgs")
    }
    val frame = Frame(cl, sp - numArgs)
    pushFrame(frame)
    sp = frame.basePointer + cl.fn.numLocals
  }

  private def executeIndexExpression(left: MObject, index: MObject): Unit = {
    (left, index) match {
      case (a: MArray, i: MInteger) => executeArrayIndex(a, i)
      case (h: MHash, _) => executeHashIndex(h, index)
      case _ => throw VMException(s"index operator not supported ${left.typeDesc()}")
    }
  }

  private def executeArrayIndex(array: MArray, index: MInteger): Unit = {
    val i = index.value
    val max = array.elements.length - 1
    if (i < 0 || i > max) {
      push(Null)
    } else {
      push(array.elements(i.toInt).get)
    }
  }

  private def executeHashIndex(hash: MHash, index: MObject): Unit = {
    index match {
      case h: MHashable[?] =>
        hash.pairs.get(h.hashKey()) match {
          case Some(pair) => push(pair.value)
          case None => push(Null)
        }
      case _ => throw VMException(s"unusable as hash key: ${index.typeDesc()}")
    }
  }

  private def executeMinusOperator(): Unit = {
    val operand = pop()
    operand match {
      case Some(i: MInteger) => push(-i)
      case _ => throw VMException(s"unsupported type for negation: ${operand.typeDesc()}")
    }
  }

  private def executeComparison(op: Opcode): Unit = {
    val right = pop()
    val left = pop()
    (left, op, right) match {
      case (Some(l: MInteger), _, Some(r: MInteger)) => executeBinaryIntegerComparison(op, l, r)
      case (Some(l: MObject), OpEqual, Some(r: MObject)) => push((l == r).toMBoolean)
      case (Some(l: MObject), OpNotEqual, Some(r: MObject)) => push((l != r).toMBoolean)
      case (_, _, _) => throw VMException(s"unknown operator $op (${left.typeDesc()} ${right.typeDesc()})")
    }
  }

  private def executeBinaryIntegerComparison(op: Opcode, left: MInteger, right: MInteger): Unit = {
    val l = left.value
    val r = right.value
    op match {
      case OpEqual => push((l == r).toMBoolean)
      case OpNotEqual => push((l != r).toMBoolean)
      case OpGreaterThan => push((l > r).toMBoolean)
      case _ => throw VMException(s"unknown operator $op")
    }
  }

  private def executeBinaryOperation(op: Opcode): Unit = {
    val right = pop()
    val left = pop()
    (left, right) match {
      case (Some(l: MInteger), Some(r: MInteger)) => executeBinaryIntegerOperation(op, l, r)
      case (Some(l: MString), Some(r: MString)) => executeBinaryStringOperation(op, l, r)
      case (_, _) => throw VMException(s"unsupported types for binary operation ${left.typeDesc()} ${right.typeDesc()}")
    }
  }

  private def executeBinaryStringOperation(opcode: Opcode, left: MString, right: MString): Unit = {
    opcode match {
      case OpAdd => push(MString(left.value + right.value))
      case _ => throw VMException(s"unknown string operator $opcode")
    }
  }

  private def executeBinaryIntegerOperation(opcode: Opcode, left: MInteger, right: MInteger): Unit = {
    val result = opcode match {
      case OpAdd => left + right
      case OpSub => left - right
      case OpMul => left * right
      case OpDiv => left / right
      case _ => throw VMException("unknown integer operator $op")
    }
    push(result)
  }

  private def pop(): Option[MObject] = {
    stackPop().also { _ => sp -= 1 }
  }

  private def stackPop(): Option[MObject] = {
    if (sp == 0) {
      None
    } else {
      stack(sp - 1)
    }
  }

  def push(obj: MObject): Unit = {
    if (sp >= STACK_SIZE) {
      throw VMException("stack overflow")
    }
    stack(sp) = Some(obj)
    sp += 1
  }

  private def pushClosure(index: Int, numFree: Int): Unit = {
    constants(index) match {
      case cf: MCompiledFunction =>
        var i = 0
        val free = Array.fill[MObject](numFree) {
          val maybe = stack(sp - numFree - i)
          i += 1
          maybe.get
        }.toList
        sp -= numFree
        push(MClosure(cf, free))
      case c: _ => throw VMException(s"not a function $c")
    }
  }

  private def pushFrame(frame: Frame): Unit = {
    frames(frameIndex) = Some(frame)
    frameIndex += 1
  }

  private def popFrame(): Frame = {
    frameIndex -= 1
    frames(frameIndex).get
  }

  def lastPoppedStackElem: Option[MObject] = stack(sp)

  extension (b: Boolean) {
    def toMBoolean: MBoolean = if (b) {
      True
    } else {
      False
    }
  }

  extension (o: Option[MObject]) {
    def isTruthy: Boolean = {
      o match {
        case Some(b: MBoolean) => b.value
        case Some(_: MNull.type) => false
        case _ => true
      }
    }
  }

  extension (globals: mutable.ListBuffer[MObject]) {
    def update(index: Int, maybe: Option[MObject]): Unit = {
      maybe.foreach { obj =>
        if (index == globals.length) {
          globals.addOne(obj)
        } else if (index < globals.length) {
          globals(index) = obj
        }
      }
    }
  }
}

object VM {
  inline val STACK_SIZE = 2048
  inline val MAX_FRAME_SIZE = 1024
  val True: MBoolean = MBoolean(true)
  val False: MBoolean = MBoolean(false)
  val Null: MNull.type = MNull
}

class VMException(message: String) extends Exception(message)
