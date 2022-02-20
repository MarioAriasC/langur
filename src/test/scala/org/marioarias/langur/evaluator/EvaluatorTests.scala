package org.marioarias.langur.evaluator

import org.marioarias.langur.*
import org.marioarias.langur.lexer.Lexer
import org.marioarias.langur.objects.*
import org.marioarias.langur.parser.Parser
import utest.{ArrowAssert, TestSuite, Tests, test}

import scala.reflect.{ClassTag, classTag}

/**
 * Created by IntelliJ IDEA.
 *
 * @author Mario Arias
 *         Date: 19/2/22
 *         Time: 3:31 PM
 */
object EvaluatorTests extends TestSuite {

  extension (l: List[(String, Int)]) {
    def eval(): Unit = {
      l.foreach { case (input, expected) =>
        println(input)
        val evaluated = testEval(input)
        println(evaluated)
        testInteger(evaluated, expected.toLong)
      }
    }
  }

  override def tests: Tests = Tests {
    test("eval integer expression") {
      List(
        ("5", 5),
        ("10", 10),
        ("-5", -5),
        ("-10", -10),
        ("5 + 5 + 5 + 5 - 10", 10),
        ("2 * 2 * 2 * 2 * 2", 32),
        ("-50 + 100 + -50", 0),
        ("5 * 2 + 10", 20),
        ("5 + 2 * 10", 25),
        ("20 + 2 * -10", 0),
        ("50 / 2 * 2 + 10", 60),
        ("2 * (5 + 10)", 30),
        ("3 * 3 * 3 + 10", 37),
        ("3 * (3 * 3) + 10", 37),
        ("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50)
      ).eval()
    }

    test("eval boolean expression") {
      List(
        ("true", true),
        ("false", false),
        ("1 < 2", true),
        ("1 > 2", false),
        ("1 < 1", false),
        ("1 > 1", false),
        ("1 == 1", true),
        ("1 != 1", false),
        ("1 == 2", false),
        ("1 != 2", true),
        ("true == true", true),
        ("false == false", true),
        ("true == false", false),
        ("true != false", true),
        ("false != true", true),
        ("(1 < 2) == true", true),
        ("(1 < 2) == false", false),
        ("(1 > 2) == true", false),
        ("(1 > 2) == false", true)
      ).foreach { case (input, expected) =>
        val evaluated = testEval(input)
        testBoolean(evaluated, expected)
      }
    }

    test("bang operator") {
      List(
        ("!true", false),
        ("!false", true),
        ("!5", false),
        ("!!true", true),
        ("!!false", false),
        ("!!5", true)
      ).foreach { case (input, expected) =>
        val evaluated = testEval(input)
        testBoolean(evaluated, expected)
      }
    }

    test("if else expression") {
      List(
        ("if (true) { 10 }", Some(10)),
        ("if (false) { 10 }", None),
        ("if (1) { 10 }", Some(10)),
        ("if (1 < 2) { 10 }", Some(10)),
        ("if (1 > 2) { 10 }", None),
        ("if (1 > 2) { 10 } else { 20 }", Some(20)),
        ("if (1 < 2) { 10 } else { 20 }", Some(10))
      ).foreach { case (input, expected) =>
        val evaluated = testEval(input)
        expected match {
          case Some(l: Int) => testInteger(evaluated, l.toLong)
          case None => evaluated ==> Some(Evaluator.NULL)
        }
      }
    }

    test("return statement") {
      List(("return 10;", 10),
        ("return 10; 9;", 10),
        ("return 2 * 5; 9;", 10),
        ("9; return 2 * 5; 9;", 10),
        (
          """
          if (10 > 1) {
            if (10 > 1) {
              return 10;
            }

            return 1;
          }
          """, 10
        ),
        (
          """
          let f = fn(x) {
            return x;
            x + 10;
          };
          f(10);""", 10
        ),
        (
          """
          let f = fn(x) {
             let result = x + 10;
             return result;
             return 10;
          };
          f(10);""", 20
        )).eval()
    }
  }

  private def testInteger(obj: Option[MObject], expected: Long): Unit = {
    obj match {
      case Some(o: MObject) => o.asInstanceOf[MInteger].value ==> expected
      case _ => fail(s"obj is not MInteger. got=$obj")
    }
  }

  private def testBoolean(obj: Option[MObject], expected: Boolean): Unit = {
    obj match {
      case Some(o: MObject) => o.asInstanceOf[MBoolean].value ==> expected
      case _ => fail(s"obj is not MBoolean. got=$obj")
    }
  }

  private def testEval(input: String): Option[MObject] = {
    //    println(input)
    val lexer = Lexer(input)
    val parser = Parser(lexer)
    val program = parser.parseProgram()
    if (parser.errors().nonEmpty) {
      parser.errors().foreach(println(_))
    }

    Evaluator.eval(Some(program), Environment.newEnvironment())
  }
}
