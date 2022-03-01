package org.marioarias.langur.compiler

import org.marioarias.langur.*
import org.marioarias.langur.code.*
import org.marioarias.langur.objects.{MCompiledFunction, MObject, typeDesc}
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
    test("boolean expressions") {
      List(
        CTC(
          "true",
          List(),
          List(
            make(OpTrue),
            make(OpPop)
          )
        ),
        CTC(
          "false",
          List(),
          List(
            make(OpFalse),
            make(OpPop)
          )
        ),
        CTC(
          "1 > 2",
          List(1, 2),
          List(
            make(OpConstant, 0),
            make(OpConstant, 1),
            make(OpGreaterThan),
            make(OpPop)
          )
        ),
        CTC(
          "1 < 2",
          List(2, 1),
          List(
            make(OpConstant, 0),
            make(OpConstant, 1),
            make(OpGreaterThan),
            make(OpPop)
          )
        ),
        CTC(
          "1 == 2",
          List(1, 2),
          List(
            make(OpConstant, 0),
            make(OpConstant, 1),
            make(OpEqual),
            make(OpPop)
          )
        ),
        CTC(
          "1 != 2",
          List(1, 2),
          List(
            make(OpConstant, 0),
            make(OpConstant, 1),
            make(OpNotEqual),
            make(OpPop)
          )
        ),
        CTC(
          "true == false",
          List(),
          List(
            make(OpTrue),
            make(OpFalse),
            make(OpEqual),
            make(OpPop)
          )
        ),
        CTC(
          "true != false",
          List(),
          List(
            make(OpTrue),
            make(OpFalse),
            make(OpNotEqual),
            make(OpPop)
          )
        ),
        CTC(
          "!true",
          List(),
          List(
            make(OpTrue),
            make(OpBang),
            make(OpPop)
          )
        )
      ).runCompilerTests()
    }
    test("conditionals") {
      List(
        CTC(
          "if (true) {10}; 3333;",
          List(10, 3333),
          List(
            make(OpTrue),
            make(OpJumpNotTruthy, 10),
            make(OpConstant, 0),
            make(OpJump, 11),
            make(OpNull),
            make(OpPop),
            make(OpConstant, 1),
            make(OpPop)
          )
        ),
        CTC(
          "if (true) {10} else {20}; 3333;",
          List(10, 20, 3333),
          List(
            make(OpTrue),
            make(OpJumpNotTruthy, 10),
            make(OpConstant, 0),
            make(OpJump, 13),
            make(OpConstant, 1),
            make(OpPop),
            make(OpConstant, 2),
            make(OpPop)
          )
        )
      ).runCompilerTests()
    }
    test("global let statement") {
      List(
        CTC(
          "let one = 1; let two = 2;",
          List(1, 2),
          List(
            make(OpConstant, 0),
            make(OpSetGlobal, 0),
            make(OpConstant, 1),
            make(OpSetGlobal, 1),
          )
        ),
        CTC(
          "let one = 1; one;",
          List(1),
          List(
            make(OpConstant, 0),
            make(OpSetGlobal, 0),
            make(OpGetGlobal, 0),
            make(OpPop),
          )
        ),
        CTC(
          "let one = 1; let two = one; two;",
          List(1),
          List(
            make(OpConstant, 0),
            make(OpSetGlobal, 0),
            make(OpGetGlobal, 0),
            make(OpSetGlobal, 1),
            make(OpGetGlobal, 1),
            make(OpPop),
          )
        )
      ).runCompilerTests()
    }
    test("string expressions") {
      List(
        CTC(
          """"monkey"""",
          List("monkey"),
          List(
            make(OpConstant, 0),
            make(OpPop)
          )
        ),
        CTC(
          """"mon" + "key"""",
          List("mon", "key"),
          List(
            make(OpConstant, 0),
            make(OpConstant, 1),
            make(OpAdd),
            make(OpPop)
          )
        )
      ).runCompilerTests()
    }
    test("array literals") {
      List(
        CTC(
          "[]",
          List(),
          List(
            make(OpArray, 0),
            make(OpPop)
          )
        ),
        CTC(
          "[1, 2, 3]",
          List(1, 2, 3),
          List(
            make(OpConstant, 0),
            make(OpConstant, 1),
            make(OpConstant, 2),
            make(OpArray, 3),
            make(OpPop)
          )
        ),
        CTC(
          "[1 + 2, 3 - 4, 5 * 6]",
          List(1, 2, 3, 4, 5, 6),
          List(
            make(OpConstant, 0),
            make(OpConstant, 1),
            make(OpAdd),
            make(OpConstant, 2),
            make(OpConstant, 3),
            make(OpSub),
            make(OpConstant, 4),
            make(OpConstant, 5),
            make(OpMul),
            make(OpArray, 3),
            make(OpPop)
          )
        ),
      ).runCompilerTests()
    }
    test("hash literal") {
      List(
        CTC(
          "{}",
          List(),
          List(
            make(OpHash, 0),
            make(OpPop)
          )
        ),
        CTC(
          "{1: 2, 3: 4, 5: 6}",
          List(1, 2, 3, 4, 5, 6),
          List(
            make(OpConstant, 0),
            make(OpConstant, 1),
            make(OpConstant, 2),
            make(OpConstant, 3),
            make(OpConstant, 4),
            make(OpConstant, 5),
            make(OpHash, 6),
            make(OpPop)
          )
        ),
        CTC(
          "{1: 2 + 3, 4: 5 * 6}",
          List(1, 2, 3, 4, 5, 6),
          List(
            make(OpConstant, 0),
            make(OpConstant, 1),
            make(OpConstant, 2),
            make(OpAdd),
            make(OpConstant, 3),
            make(OpConstant, 4),
            make(OpConstant, 5),
            make(OpMul),
            make(OpHash, 4),
            make(OpPop)
          )
        )
      ).runCompilerTests()
    }
    test("index expression") {
      List(
        CTC(
          "[1, 2, 3][1 + 1]",
          List(1, 2, 3, 1, 1),
          List(
            make(OpConstant, 0),
            make(OpConstant, 1),
            make(OpConstant, 2),
            make(OpArray, 3),
            make(OpConstant, 3),
            make(OpConstant, 4),
            make(OpAdd),
            make(OpIndex),
            make(OpPop)
          )
        ),
        CTC(
          "{1: 2}[2 - 1]",
          List(1, 2, 2, 1),
          List(
            make(OpConstant, 0),
            make(OpConstant, 1),
            make(OpHash, 2),
            make(OpConstant, 2),
            make(OpConstant, 3),
            make(OpSub),
            make(OpIndex),
            make(OpPop)
          )
        )
      ).runCompilerTests()
    }
    test("functions") {
      List(
        CTC(
          "fn() {return 5 + 10 }",
          List(
            5, 10, List(
              make(OpConstant, 0),
              make(OpConstant, 1),
              make(OpAdd),
              make(OpReturnValue)
            )
          ),
          List(
            make(OpClosure, 2, 0),
            make(OpPop)
          )
        ),
        CTC(
          "fn() { 5 + 10 }",
          List(
            5, 10, List(
              make(OpConstant, 0),
              make(OpConstant, 1),
              make(OpAdd),
              make(OpReturnValue)
            )
          ),
          List(
            make(OpClosure, 2, 0),
            make(OpPop)
          )
        ),
        CTC(
          "fn() {1; 2}",
          List(
            1, 2, List(
              make(OpConstant, 0),
              make(OpPop),
              make(OpConstant, 1),
              make(OpReturnValue),
            )
          ),
          List(
            make(OpClosure, 2, 0),
            make(OpPop),
          ),
        )
      ).runCompilerTests()
    }
  }

  extension[T] (tests: List[CTC[T]]) {
    def runCompilerTests(): Unit = {
      tests.foreach { case CTC(input, expectedConstants, expectedInstructions) =>
        println(s"input = $input")
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
        case s: String => testStringObject(s, actual(i))
        case l: List[?] =>
          actual(i) match {
            case cf: MCompiledFunction => testInstructions(l.asInstanceOf[List[Instructions]], cf.instructions)
            case act: _ => fail(s"constant $act - not a function, got = ${act.typeDesc()}")
          }
      }
    }
  }
}
