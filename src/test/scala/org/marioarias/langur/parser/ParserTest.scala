package org.marioarias.langur.parser

import org.marioarias.langur.ast.*
import org.marioarias.langur.lexer.Lexer
import org.marioarias.langur.{fail, isType}
import utest.{ArrowAssert, TestSuite, Tests, test}

/** Created by IntelliJ IDEA.
  *
  * @author
  *   Mario Arias Date: 1/2/22 Time: 1:56 PM
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
    isType(statement) { (letStatement: LetStatement) =>
      letStatement.name.value ==> expectedIdentifier
      letStatement.name.tokenLiteral() ==> expectedIdentifier
    }
  }

  private def testLiteralExpression(
      value: Option[Expression],
      expectedValue: Any
  ): Unit = {
    expectedValue match {
      case v: Long    => testLongLiteral(value, v)
      case v: Integer => testLongLiteral(value, v.toLong)
      case b: Boolean => testBooleanLiteral(value, b)
      case s: String  => testIdentifier(value, s)
      case _ =>
        fail(
          s"type value not handled. got=${expectedValue.getClass.getSimpleName}"
        )
    }
  }

  private def testLongLiteral(expression: Option[Expression], l: Long): Unit = {
    isType(expression) { (maybe: Some[IntegerLiteral]) =>
      val exp = maybe.value
      exp.value ==> l
      exp.tokenLiteral() ==> l.toString
    }
  }

  private def testBooleanLiteral(
      expression: Option[Expression],
      b: Boolean
  ): Unit = {
    isType(expression) { (maybe: Some[BooleanLiteral]) =>
      val exp = maybe.value
      exp.value ==> b
      exp.tokenLiteral() ==> b.toString
    }
  }

  private def testIdentifier(
      expression: Option[Expression],
      s: String
  ): Unit = {
    isType(expression) { (maybe: Some[Identifier]) =>
      val exp = maybe.value
      exp.value ==> s
      exp.tokenLiteral() ==> s
    }
  }
}
