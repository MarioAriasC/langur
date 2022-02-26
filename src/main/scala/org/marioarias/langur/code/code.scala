package org.marioarias.langur.code

import org.marioarias.langur.utils.Utils.also

class UnsignedByte(val byte: Short) extends AnyVal {
  override def toString: String = s"${byte}u"

  def &(i: Int): Int = byte & i

  def toInt:Int = byte
}

class Definition(val name: String, val operandsWidths: Array[Int] = Array.emptyIntArray) {

  def canEqual(other: Any): Boolean = other.isInstanceOf[Definition]

  override def equals(other: Any): Boolean = other match {
    case that: Definition =>
      (that canEqual this) &&
        name == that.name &&
        (operandsWidths sameElements that.operandsWidths)
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(name, operandsWidths)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }


  override def toString = s"Definition($name, ${operandsWidths.mkString("Array(", ", ", ")")})"
}

extension (s: String) {
  def toDefinition(operandsWidths: Int*) = Definition(s, operandsWidths.toArray)
}

type UB = UnsignedByte
type Opcode = UB

type Instructions = Array[UB]

extension (i: Instructions) {
  def offset(offset: Int): Instructions = i.slice(offset, i.length)

  def writeChar(offset: Int, v: Int): Unit = {
    i(offset) = ((v >>> 8) & 255).u
    i(offset + 1) = ((v >>> 0) & 255).u
  }

  def readInt(offset: Int): Int = i.offset(offset).readChar

  def readChar: Char = {
    val ch1 = i.read(0)
    val ch2 = i.read(1)
    if ((ch1 | ch2) < 0) {
      throw IllegalStateException()
    } else {
      ((ch1 << 8) + (ch2 << 0)).toChar
    }
  }

  def readByte: UB ={
    val ch = read(0)
    if(ch < 0){
      throw IllegalStateException()
    } else {
      ch.u
    }
  }

  def read(position: Int): Int = i(position) & 255
}

extension (i: Int) {
  def u: UB = UnsignedByte(i.toShort)
}

val OpConstant: Opcode = 0.u
val OpAdd: Opcode = 1.u
val OpPop: Opcode = 2.u
val OpSub: Opcode = 3.u
val OpMul: Opcode = 4.u
val OpDiv: Opcode = 5.u
val OpTrue: Opcode = 6.u
val OpFalse: Opcode = 7.u
val OpEqual: Opcode = 8.u
val OpNotEqual: Opcode = 9.u
val OpGreaterThan: Opcode = 10.u
val OpMinus: Opcode = 11.u
val OpBang: Opcode = 12.u
val OpJumpNotTruthy: Opcode = 13.u
val OpJump: Opcode = 14.u
val OpNull: Opcode = 15.u
val OpGetGlobal: Opcode = 16.u
val OpSetGlobal: Opcode = 17.u
val OpArray: Opcode = 18.u
val OpHash: Opcode = 19.u
val OpIndex: Opcode = 20.u
val OpCall: Opcode = 21.u
val OpReturnValue: Opcode = 22.u
val OpReturn: Opcode = 23.u
val OpGetLocal: Opcode = 24.u
val OpSetLocal: Opcode = 25.u
val OpGetBuiltin: Opcode = 26.u
val OpClosure: Opcode = 27.u
val OpGetFree: Opcode = 28.u
val OpCurrentClosure: Opcode = 29.u

val definitions = Map(
  OpConstant -> "OpConstant".toDefinition(2),
  OpAdd -> "OpAdd".toDefinition(),
  OpPop -> "OpPop".toDefinition(),
  OpSub -> "OpSub".toDefinition(),
  OpMul -> "OpMul".toDefinition(),
  OpDiv -> "OpDiv".toDefinition(),
  OpTrue -> "OpTrue".toDefinition(),
  OpFalse -> "OpFalse".toDefinition(),
  OpEqual -> "OpEqual".toDefinition(),
  OpNotEqual -> "OpNotEqual".toDefinition(),
  OpGreaterThan -> "OpGreaterThan".toDefinition(),
  OpMinus -> "OpMinus".toDefinition(),
  OpBang -> "OpBang".toDefinition(),
  OpJumpNotTruthy -> "OpJumpNotTruthy".toDefinition(2),
  OpJump -> "OpJump".toDefinition(2),
  OpNull -> "OpNull".toDefinition(),
  OpGetGlobal -> "OpGetGlobal".toDefinition(2),
  OpSetGlobal -> "OpSetGlobal".toDefinition(2),
  OpArray -> "OpArray".toDefinition(2),
  OpHash -> "OpHash".toDefinition(2),
  OpIndex -> "OpIndex".toDefinition(),
  OpCall -> "OpCall".toDefinition(1),
  OpReturnValue -> "OpReturnValue".toDefinition(),
  OpReturn -> "OpReturn".toDefinition(),
  OpGetLocal -> "OpGetLocal".toDefinition(1),
  OpSetLocal -> "OpSetLocal".toDefinition(1),
  OpGetBuiltin -> "OgGetBuiltin".toDefinition(1),
  OpClosure -> "OpClosure".toDefinition(2, 1),
  OpGetFree -> "OpGetFree".toDefinition(1),
  OpCurrentClosure -> "OpCurrentClosure".toDefinition()
)

def lookup(op: Opcode): Definition = {
  definitions.getOrElse(op, throw IllegalArgumentException(s"opcode $op undefined"))
}

def make(op: Opcode, operands: Int*): Instructions = {
  try {
    val definition = lookup(op)
    val instructionLength = definition.operandsWidths.sum + 1
    val instruction = new Array[UB](instructionLength)
    instruction(0) = op
    var offset = 1
    operands.zipWithIndex.foreach { case (operand, i) =>
      val width = definition.operandsWidths(i)
      width match {
        case 2 => instruction.writeChar(offset, operand)
        case 1 => instruction(offset) = operand.u
      }
      offset += width
    }

    instruction
  } catch {
    case _: IllegalArgumentException => Array[UB]()
  }
}