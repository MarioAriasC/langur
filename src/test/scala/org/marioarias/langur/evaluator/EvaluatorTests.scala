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
        //        println(input)
        val evaluated = testEval(input)
        //        println(evaluated)
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

    test("error handling") {
      List(
        (
          "5 + true;",
          "type mismatch: MInteger + MBoolean",
        ),
        (
          "5 + true; 5;",
          "type mismatch: MInteger + MBoolean",
        ),
        (
          "-true",
          "unknown operator: -MBoolean",
        ),
        (
          "true + false;",
          "unknown operator: MBoolean + MBoolean",
        ),
        (
          "true + false + true + false;",
          "unknown operator: MBoolean + MBoolean",
        ),
        (
          "5; true + false; 5",
          "unknown operator: MBoolean + MBoolean",
        ),
        (
          "if (10 > 1) { true + false; }",
          "unknown operator: MBoolean + MBoolean",
        ),
        (
          """
          if (10 > 1) {
            if (10 > 1) {
              return true + false;
            }

            return 1;
          }
          """,
          "unknown operator: MBoolean + MBoolean",
        ),
        (
          "foobar",
          "identifier not found: foobar",
        ),
        (
          """"Hello" - "World"""",
          "unknown operator: MString - MString"
        ),
        (
          """{"name": "Monkey"}[fn(x) {x}];""",
          "unusable as a hash key: MFunction"
        )
      ).foreach { case (input, expected) =>
        val evaluated = testEval(input)
        checkType(evaluated) { (error: MError) =>
          error.message ==> expected
        }
      }
    }

    test("let statement") {
      List(
        ("let a = 5; a;", 5),
        ("let a = 5 * 5; a;", 25),
        ("let a = 5; let b = a; b;", 5),
        ("let a = 5; let b = a; let c = a + b + 5; c;", 15)
      ).eval()
    }

    test("function object") {
      val input = "fn(x) { x + 2; };"
      val evaluated = testEval(input)
      checkType(evaluated) { (fn: MFunction) =>
        for parameters <- fn.parameters yield {
          parameters.size ==> 1
          parameters.head.toString ==> "x"
        }
        fn.body.map(_.toString).getOrElse("") ==> "(x + 2)"
      }
    }

    test("function application") {
      List(
        ("let identity = fn(x) { x; }; identity(5);", 5),
        ("let identity = fn(x) { return x; }; identity(5);", 5),
        ("let double = fn(x) { x * 2; }; double(5);", 10),
        ("let add = fn(x, y) { x + y; }; add(5, 5);", 10),
        ("let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));", 20),
        ("fn(x) { x; }(5)", 5),
      ).eval()
    }

    test("enclosing environment") {
      val input =
        """let first = 10;
           let second = 10;
           let third = 10;

           let ourFunction = fn(first) {
             let second = 20;

             first + second + third;
           };

           ourFunction(20) + first + second;"""
      testInteger(testEval(input), 70L)
    }

    test("string literal") {
      val input = """"Hello World!""""
      testString(testEval(input), "Hello World!")
    }

    test("string concatenation") {
      val input = """"Hello" + " " + "World!""""
      testString(testEval(input), "Hello World!")
    }

    test("builtin functions") {
      List(
        ("""len("")""", Some(0)),
        ("""len("four")""", Some(4)),
        ("""len("hello world")""", Some(11)),
        ("len(1)", Some("argument to `len` not supported, got MInteger")),
        ("""len("one", "two")""", Some("wrong number of arguments. got=2, want=1")),
        ("len([1, 2, 3])", Some(3)),
        ("len([])", Some(0)),
        ("push([], 1)", Some(List(1))),
        ("push(1, 1)", Some("argument to `push` must be ARRAY, got MInteger")),
        ("first([1, 2, 3])", Some(1)),
        ("first([])", None),
        ("first(1)", Some("argument to `first` must be ARRAY, got MInteger")),
        ("last([1, 2, 3])", Some(3)),
        ("last([])", None),
        ("last(1)", Some("argument to `last` must be ARRAY, got MInteger")),
        ("rest([1, 2, 3])", Some(List(2, 3))),
        ("rest([])", None),
      ).foreach { case (input, expected) =>
        val evaluated = testEval(input)
        expected match {
          case None => evaluated ==> Some(Evaluator.NULL)
          case Some(i: Int) => testInteger(evaluated, i.toLong)
          case Some(s: String) => checkType(evaluated) { (error: MError) =>
            Some(error.message) ==> expected
          }
          case Some(l: List[Int]) => checkType(evaluated) { (array: MArray) =>
            l.size ==> array.elements.size
            l.zipWithIndex.foreach { case (element, i) =>
              testInteger(array.elements(i), element.toLong)
            }
          }
        }
      }
    }
  }

  private def testString(obj: Option[MObject], expected: String): Unit = {
    obj match {
      case Some(o: MString) => o.value ==> expected
      case _ => fail(s"obj is not MString. got=$obj")
    }
  }

  private def testInteger(obj: Option[MObject], expected: Long): Unit = {
    obj match {
      case Some(o: MInteger) => o.value ==> expected
      case _ => fail(s"obj is not MInteger. got=$obj")
    }
  }

  private def testBoolean(obj: Option[MObject], expected: Boolean): Unit = {
    obj match {
      case Some(o: MBoolean) => o.value ==> expected
      case _ => fail(s"obj is not MBoolean. got=$obj")
    }
  }


  private def testEval(input: String): Option[MObject] = {
    //    println(input)
    val lexer = Lexer(input)
    val parser = Parser(lexer)
    val program = parser.parseProgram()
    if (parser.errors().nonEmpty) {
      fail(parser.errors().mkString("\n"))
    }

    Evaluator.eval(Some(program), Environment.newEnvironment())
  }
}
