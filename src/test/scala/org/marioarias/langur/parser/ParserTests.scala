package org.marioarias.langur.parser

import org.marioarias.langur.ast.*
import org.marioarias.langur.lexer.Lexer
import org.marioarias.langur.{checkType, fail}
import utest.{ArrowAssert, TestSuite, Tests, test}

/** Created by IntelliJ IDEA.
 *
 * @author
 * Mario Arias Date: 1/2/22 Time: 1:56 PM
 */
object ParserTests extends TestSuite {

  override def tests: Tests = Tests {
    test("let statement") {
      List(
        ("let x = 5;", "x", 5),
        ("let y = true;", "y", true),
        ("let foobar = y;", "foobar", "y")
      ).foreach { case (input, expectedIdentifier, expectedValue) =>
        val program = createProgram(input)
        countStatements(1, program)
        val statement = program.statements.head
        testLetStatement(statement, expectedIdentifier)
        val value = statement.asInstanceOf[LetStatement].value
        testLiteralExpression(value, expectedValue)
      }
    }

    test("return statements") {
      List(
        ("return 5;", 5),
        ("return true;", true),
        ("return foobar;", "foobar")
      ).foreach { case (input, expectedValue) =>
        val program = createProgram(input)
        countStatements(1, program)
        checkType(program.statements.head) { (statement: ReturnStatement) =>
          statement.tokenLiteral() ==> "return"
          testLiteralExpression(statement.returnValue, expectedValue)
        }
      }
    }

    test("identifier expression") {
      val input = "foobar;"
      val program = createProgram(input)
      countStatements(1, program)
      checkType(program.statements.head) { (statement: ExpressionStatement) =>
        checkType(statement.expression) { (identifier: Identifier) =>
          identifier.value ==> "foobar"
          identifier.tokenLiteral() ==> "foobar"
        }
      }
    }

    test("integer literals") {
      val input = "5;"
      val program = createProgram(input)
      countStatements(1, program)
      checkType(program.statements.head) { (statement: ExpressionStatement) =>
        statement.expression match {
          case Some(i: IntegerLiteral) =>
            i.value ==> 5.toLong
            i.tokenLiteral() ==> "5"
          case e => fail(s"statement.expression not IntegerLiteral .got=${e.getClass.getSimpleName}")
        }
      }
    }

    test("parsing prefix expression") {
      List(
        ("!5;", "!", 5),
        ("-15;", "-", 15),
        ("!true;", "!", true),
        ("!false;", "!", false)
      ).foreach { case (input, operator, value) =>
        val program = createProgram(input)
        countStatements(1, program)
        checkType(program.statements.head) { (statement: ExpressionStatement) =>
          checkType(statement.expression) { (expression: PrefixExpression) =>
            expression.operator ==> operator
            testLiteralExpression(expression.right, value)
          }
        }
      }
    }

    test("parsing infix expressions") {
      List(
        ("5 + 5;", 5, "+", 5),
        ("5 - 5;", 5, "-", 5),
        ("5 * 5;", 5, "*", 5),
        ("5 / 5;", 5, "/", 5),
        ("5 > 5;", 5, ">", 5),
        ("5 < 5;", 5, "<", 5),
        ("5 == 5;", 5, "==", 5),
        ("5 != 5;", 5, "!=", 5),
        ("true == true", true, "==", true),
        ("true != false", true, "!=", false),
        ("false == false", false, "==", false)
      ).foreach { case (input, leftValue, operator, rightValue) =>
        val program = createProgram(input)
        countStatements(1, program)
        checkType(program.statements.head) { (statement: ExpressionStatement) =>
          testInfixExpression(statement.expression, leftValue, operator, rightValue)
        }
      }
    }

    test("operator precedence") {
      List(
        (
          "-a * b",
          "((-a) * b)",
        ),
        (
          "!-a",
          "(!(-a))",
        ),
        (
          "a + b + c",
          "((a + b) + c)",
        ),
        (
          "a + b - c",
          "((a + b) - c)",
        ),
        (
          "a * b * c",
          "((a * b) * c)",
        ),
        (
          "a * b / c",
          "((a * b) / c)",
        ),
        (
          "a + b / c",
          "(a + (b / c))",
        ),
        (
          "a + b * c + d / e - f",
          "(((a + (b * c)) + (d / e)) - f)",
        ),
        (
          "3 + 4; -5 * 5",
          "(3 + 4)((-5) * 5)",
        ),
        (
          "5 > 4 == 3 < 4",
          "((5 > 4) == (3 < 4))",
        ),
        (
          "5 < 4 != 3 > 4",
          "((5 < 4) != (3 > 4))",
        ),
        (
          "3 + 4 * 5 == 3 * 1 + 4 * 5",
          "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
        ),
        (
          "true",
          "true",
        ),
        (
          "false",
          "false",
        ),
        (
          "3 > 5 == false",
          "((3 > 5) == false)",
        ),
        (
          "3 < 5 == true",
          "((3 < 5) == true)",
        ),
        (
          "1 + (2 + 3) + 4",
          "((1 + (2 + 3)) + 4)",
        ),
        (
          "(5 + 5) * 2",
          "((5 + 5) * 2)",
        ),
        (
          "2 / (5 + 5)",
          "(2 / (5 + 5))",
        ),
        (
          "(5 + 5) * 2 * (5 + 5)",
          "(((5 + 5) * 2) * (5 + 5))",
        ),
        (
          "-(5 + 5)",
          "(-(5 + 5))",
        ),
        (
          "!(true == true)",
          "(!(true == true))",
        ),
        (
          "a + add(b * c) + d",
          "((a + add((b * c))) + d)",
        ),
        (
          "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
          "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
        ),
        (
          "add(a + b + c * d / f + g)",
          "add((((a + b) + ((c * d) / f)) + g))",
        ),
        (
          "a * [1, 2, 3, 4][b * c] * d",
          "((a * ([1, 2, 3, 4][(b * c)])) * d)",
        ),
        (
          "add(a * b[2], b[1], 2 * [1, 2][1])",
          "add((a * (b[2])), (b[1]), (2 * ([1, 2][1])))",
        )
      ).foreach { case (input, expected) =>
        val program = createProgram(input)
        program.toString ==> expected
      }
    }

    test("boolean expression") {
      List(
        ("true", true),
        ("false", false)
      ).foreach { case (input, expectedBoolean) =>
        val program = createProgram(input)
        countStatements(1, program)
        checkType(program.statements.head) { (statement: ExpressionStatement) =>
          checkType(statement.expression) { (boolean: BooleanLiteral) =>
            boolean.value ==> expectedBoolean
          }
        }
      }
    }

    test("if expression") {
      val input = "if (x < y) { x }"
      val program = createProgram(input)

      countStatements(1, program)

      checkType(program.statements.head) { (statement: ExpressionStatement) =>
        checkType(statement.expression) { (exp: IfExpression) =>
          testInfixExpression(exp.condition, "x", "<", "y")
          for consequence <- exp.consequence yield {
            for statements <- consequence.statements yield {
              statements.size ==> 1
              checkType(statements.head) { (exp: ExpressionStatement) =>
                testIdentifier(exp.expression, "x")
              }
            }
          }
        }
      }
    }

    test("if else expression") {
      val input = "if (x < y) { x } else { y }"
      val program = createProgram(input)

      countStatements(1, program)

      checkType(program.statements.head) { (statement: ExpressionStatement) =>
        checkType(statement.expression) { (exp: IfExpression) =>
          testInfixExpression(exp.condition, "x", "<", "y")

          for consequence <- exp.consequence yield {
            for statements <- consequence.statements yield {
              statements.size ==> 1
              checkType(statements.head) { (exp: ExpressionStatement) =>
                testIdentifier(exp.expression, "x")
              }
            }
          }

          for alternative <- exp.alternative yield {
            for statements <- alternative.statements yield {
              statements.size ==> 1
              checkType(statements.head) { (exp: ExpressionStatement) =>
                testIdentifier(exp.expression, "y")
              }
            }
          }
        }
      }
    }

    test("function literal parsing") {
      val input = "fn(x, y) { x + y;}"
      val program = createProgram(input)
      countStatements(1, program)

      checkType(program.statements.head) { (statement: ExpressionStatement) =>
        checkType(statement.expression) { (function: FunctionLiteral) =>
          for parameters <- function.parameters yield {
            testLiteralExpression(Some(parameters.head), "x")
            testLiteralExpression(Some(parameters(1)), "y")
          }
          for body <- function.body yield {
            for statements <- body.statements yield {
              statements.size ==> 1
              checkType(statements.head) { (body: ExpressionStatement) =>
                testInfixExpression(body.expression, "x", "+", "y")
              }
            }
          }
        }
      }
    }

    test("call expression parsing") {
      val input = "add(1, 2 * 3, 4+5)"

      val program = createProgram(input)

      countStatements(1, program)

      checkType(program.statements.head) { (statement: ExpressionStatement) =>
        checkType(statement.expression) { (exp: CallExpression) =>
          testIdentifier(exp.function, "add")
          for arguments <- exp.arguments yield {
            arguments.size ==> 3
            testLiteralExpression(arguments.head, 1)
            testInfixExpression(arguments(1), 2, "*", 3)
            testInfixExpression(arguments(2), 4, "+", 5)
          }
        }
      }
    }

    test("string literal expression") {
      val input = """"hello world";"""

      val program = createProgram(input)

      countStatements(1, program)

      checkType(program.statements.head) { (statement: ExpressionStatement) =>
        checkType(statement.expression) { (literal: StringLiteral) =>
          literal.value ==> "hello world"
        }
      }
    }

    test("parsing array literal") {
      val input = "[1, 2 * 2, 3 + 3]"

      val program = createProgram(input)

      checkType(program.statements.head) { (statement: ExpressionStatement) =>
        checkType(statement.expression) { (array: ArrayLiteral) =>
          for elements <- array.elements yield {
            testLongLiteral(elements.head, 1)
            testInfixExpression(elements(1), 2, "*", 2)
            testInfixExpression(elements(2), 3, "+", 3)
          }
        }
      }
    }

    test("parsing index expression") {
      val input = "myArray[1 + 1]"

      val program = createProgram(input)

      checkType(program.statements.head) { (statement: ExpressionStatement) =>
        checkType(statement.expression) { (index: IndexExpression) =>
          testIdentifier(index.left, "myArray")
          testInfixExpression(index.index, 1, "+", 1)
        }
      }
    }

    test("hash literal string keys") {
      val input = """{"one": 1, "two": 2, "three": 3}"""
      val program = createProgram(input)
      checkType(program.statements.head) { (statement: ExpressionStatement) =>
        checkType(statement.expression) { (hash: HashLiteral) =>
          3 ==> hash.pairs.size
          val expected = Map("one" -> 1, "two" -> 2, "three" -> 3)
          
          hash.pairs.foreach { case (key, value) =>
            checkType(key) { (literal: StringLiteral) =>
              val expectedValue = expected(literal.toString())
              testLiteralExpression(Some(value), expectedValue)
            }
          }
        }
      }
    }
  }

  private def createProgram(input: String): Program = {
    val lexer = new Lexer(input)
    val parser = new Parser(lexer)
    val program = parser.parseProgram()
    val errors = parser.errors()
    if (errors.nonEmpty) {
      fail(errors.mkString(" \n"))
    }
    program
  }

  private def countStatements(i: Int, program: Program): Unit = {
    i ==> program.statements.size
  }

  private def testLetStatement(
                                statement: Statement,
                                expectedIdentifier: String
                              ): Unit = {
    statement.tokenLiteral() ==> "let"
    checkType(statement) { (letStatement: LetStatement) =>
      letStatement.name.value ==> expectedIdentifier
      letStatement.name.tokenLiteral() ==> expectedIdentifier
    }
  }

  private def testLiteralExpression(
                                     value: Option[Expression],
                                     expectedValue: Any
                                   ): Unit = {
    expectedValue match {
      case v: Long => testLongLiteral(value, v)
      case v: Integer => testLongLiteral(value, v.toLong)
      case b: Boolean => testBooleanLiteral(value, b)
      case s: String => testIdentifier(value, s)
      case _ =>
        fail(
          s"type value not handled. got=${expectedValue.getClass.getSimpleName}"
        )
    }
  }

  private def testLongLiteral(expression: Option[Expression], l: Long): Unit = {
    checkType(expression) { (exp: IntegerLiteral) =>
      exp.value ==> l
      exp.tokenLiteral() ==> l.toString
    }
  }

  private def testBooleanLiteral(
                                  expression: Option[Expression],
                                  b: Boolean
                                ): Unit = {
    checkType(expression) { (exp: BooleanLiteral) =>
      exp.value ==> b
      exp.tokenLiteral() ==> b.toString
    }
  }

  private def testIdentifier(
                              expression: Option[Expression],
                              s: String
                            ): Unit = {
    checkType(expression) { (exp: Identifier) =>
      exp.value ==> s
      exp.tokenLiteral() ==> s
    }
  }

  private def testInfixExpression[T](expression: Option[Expression], leftValue: T, operator: String, rightValue: T): Unit = {
    checkType(expression) { (exp: InfixExpression) =>
      testLiteralExpression(exp.left, leftValue)
      exp.operator ==> operator
      testLiteralExpression(exp.right, rightValue)
    }
  }
}
