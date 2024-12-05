package org.marioarias.langur.code

import org.marioarias.langur.*
import org.marioarias.langur.utils.Utils.also
import utest.{ArrowAssert, TestSuite, Tests, test}

/** Created by IntelliJ IDEA.
  *
  * @author
  *   Mario Arias Date: 24/2/22 Time: 8:30 AM
  */
object CodeTests extends TestSuite {
  override def tests: Tests = Tests {
    test("make") {
      List(
        (OpConstant, Array(65534), Array(OpConstant, 255.u, 254.u)),
        (OpAdd, Array.emptyIntArray, Array(OpAdd)),
        (OpGetLocal, Array(255), Array(OpGetLocal, 255.u))
      ).foreach { case (op, operands, expected) =>
        val instructions = make(op, operands*)
        expected.length ==> instructions.length
        expected.zipWithIndex.foreach { case (byte, i) =>
          byte ==> instructions(i)
        }
      }
    }

    test("instructions inspect") {
      val instructions: List[Instructions] = List(
        make(OpAdd),
        make(OpGetLocal, 1),
        make(OpConstant, 2),
        make(OpConstant, 65535),
        make(OpClosure, 65535, 255)
      )
      val expected =
        """0000 OpAdd
          |0001 OpGetLocal 1
          |0003 OpConstant 2
          |0006 OpConstant 65535
          |0009 OpClosure 65535 255
          |""".stripMargin
      val instruction: Instructions = instructions.concatInstructions
      expected ==> instruction.inspect
    }

    test("read operands") {
      List(
        (OpConstant, Array(65535), 2),
        (OpGetLocal, Array(255), 1),
        (OpClosure, Array(65535, 255), 3)
      ).foreach { case (op, operands, bytesRead) =>
        val instructions = make(op, operands*)
        try {
          val definition = lookup(op)
          readOperands(definition, instructions.offset(1)) match {
            case (operandsRead, read) =>
              bytesRead ==> read
              operands.zipWithIndex.foreach { case (expected, i) =>
                expected ==> operandsRead(i)
              }
          }
        } catch {
          case e: Exception => fail(s"definition not found ${e.getMessage}")
        }
      }
    }
  }

  extension (ins: Instructions) {
    def inspect: String = {
      def fmtInstruction(
          definition: Definition,
          operands: Array[Int]
      ): String = {
        val operandCount = definition.operandsWidths.length
        if (operands.length != operandCount) {
          return s"ERROR: operand len ${operands.length} does not match defined $operandCount\n"
        }
        operandCount match {
          case 0 => definition.name
          case 1 => s"${definition.name} ${operands(0)}"
          case 2 => s"${definition.name} ${operands(0)} ${operands(1)}"
          case _ => s"ERROR: unhandled operatorCount for ${definition.name}"
        }
      }

      val builder = StringBuilder()
      var i = 0
      while (i < ins.length) {
        try {
          val definition = lookup(ins(i))
          readOperands(definition, ins.offset(i + 1)) match {
            case (operands, read) =>
              builder.append(
                "%04d %s\n".format(i, fmtInstruction(definition, operands))
              )
              i += 1 + read
          }
        } catch {
          case e: IllegalArgumentException =>
            builder.append(s"ERROR: ${e.getMessage}")
        }
      }
      builder.toString()
    }
  }

  def readOperands(
      definition: Definition,
      ins: Instructions
  ): (Array[Int], Int) = {
    var offset = 0
    val operands = definition.operandsWidths.map { width =>
      (width match {
        case 2 => ins.readInt(offset)
        case 1 => ins.offset(offset).readByte.toInt
        case _ => width
      }).also { _ => offset += width }
    }
    (operands, offset)
  }
}
