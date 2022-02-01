package org.marioarias.langur.parser

import org.marioarias.langur._
import org.marioarias.langur.ast.{Expression, LetStatement, Program, Statement}
import org.marioarias.langur.lexer.Lexer
import utest.{ArrowAssert, TestSuite, Tests, test}

/**
 * Created by IntelliJ IDEA.
 *
 * @author Mario Arias
 *         Date: 1/2/22
 *         Time: 1:56 PM
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
      throw new RuntimeException(s"")
    }
    program
  }

  private def countStatements(i: Int, program: Program): Unit = {
    i ==> program.statements.size
  }

  private def testLetStatement(statement: Statement, expectedIdentifier: String): Unit = {
    statement.tokenLiteral() ==> "let"
    isType(statement, { letStatement: LetStatement =>
      letStatement.name.value ==> expectedIdentifier
      letStatement.name.tokenLiteral() ==> expectedIdentifier
    })
  }

  private def testLiteralExpression(value: Option[Expression], expectedValue: Any): Unit = {
    expectedValue match {
      case v: Long => testLongLiteral(value, v)
      case _ => fail(s"type value not handled. got=${expectedValue.getClass.getSimpleName}")
    }
  }

  private def testLongLiteral(expression: Option[Expression], l: Long): Unit = {

  }
}
