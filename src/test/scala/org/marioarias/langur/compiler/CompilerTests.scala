package org.marioarias.langur.compiler

import org.marioarias.langur.code.*
import org.marioarias.langur.utils.Utils.also
import org.marioarias.langur.objects.MObject
import org.marioarias.langur.{assertInstructions, concatInstructions, parse, testIntegerObject}
import utest.{ArrowAssert, TestSuite, Tests, test}

/**
 * Created by IntelliJ IDEA.
 *
 * @author Mario Arias
 *         Date: 26/2/22
 *         Time: 1:44 PM
 */
object CompilerTests extends TestSuite {

  case class CTC[T](input: String, expectedConstants: List[T], expectedInstructions: List[Instructions])

  override def tests: Tests = Tests {
    test("integer arithmetic") {
      List(
        CTC(
          "1 + 2",
          List(1, 2),
          List(
            make(OpConstant, 0),
            make(OpConstant, 1),
            make(OpAdd),
            make(OpPop)
          )
        ),
        CTC(
          "1; 2",
          List(1, 2),
          List(
            make(OpConstant, 0),
            make(OpPop),
            make(OpConstant, 1),
            make(OpPop)
          )
        ),
        CTC(
          "1 - 2",
          List(1, 2),
          List(
            make(OpConstant, 0),
            make(OpConstant, 1),
            make(OpSub),
            make(OpPop)
          )
        ),
        CTC(
          "1 * 2",
          List(1, 2),
          List(
            make(OpConstant, 0),
            make(OpConstant, 1),
            make(OpMul),
            make(OpPop)
          )
        ),
        CTC(
          "2 / 1",
          List(2, 1),
          List(
            make(OpConstant, 0),
            make(OpConstant, 1),
            make(OpDiv),
            make(OpPop)
          )
        ),
        CTC(
          "-1",
          List(1),
          List(
            make(OpConstant, 0),
            make(OpMinus),
            make(OpPop)
          )
        )
      ).runCompilerTests()
    }
  }

  extension[T] (tests: List[CTC[T]]) {
    def runCompilerTests(): Unit = {
      tests.foreach { case CTC(input, expectedConstants, expectedInstructions) =>
        val program = parse(input)
        val compiler = MCompiler()

        compiler.compile(program)
        val bytecode = compiler.bytecode
        testInstructions(expectedInstructions, bytecode.instructions)
        testConstants(expectedConstants, bytecode.constants)
      }
    }
  }

  private def testInstructions(expected: List[Instructions], actual: Instructions): Unit = {
    val concatenated = expected.concatInstructions
    concatenated.length ==> actual.length
    assertInstructions(concatenated, actual)
  }

  private def testConstants[T](expected: List[T], actual: List[MObject]): Unit = {
    expected.length ==> actual.length
    expected.zipWithIndex.foreach { (constant, i) =>
      constant match {
        case l: Int => testIntegerObject(l, actual(i))
      }
    }
  }
}
