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
object ParserTest extends TestSuite {

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
        checkType(statement.expression) { (maybe: Some[Identifier]) =>
          val identifier = maybe.value
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
          case e: _ => fail(s"statement.expression not IntegerLiteral .got=${e.getClass.getSimpleName}")
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
          checkType(statement.expression) { (maybe: Some[PrefixExpression]) =>
            val expression = maybe.value
            expression.operator ==> operator
            testLiteralExpression(expression.right, value)
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
    checkType(expression) { (maybe: Some[IntegerLiteral]) =>
      val exp = maybe.value
      exp.value ==> l
      exp.tokenLiteral() ==> l.toString
    }
  }

  private def testBooleanLiteral(
                                  expression: Option[Expression],
                                  b: Boolean
                                ): Unit = {
    checkType(expression) { (maybe: Some[BooleanLiteral]) =>
      val exp = maybe.value
      exp.value ==> b
      exp.tokenLiteral() ==> b.toString
    }
  }

  private def testIdentifier(
                              expression: Option[Expression],
                              s: String
                            ): Unit = {
    checkType(expression) { (maybe: Some[Identifier]) =>
      val exp = maybe.value
      exp.value ==> s
      exp.tokenLiteral() ==> s
    }
  }
}
