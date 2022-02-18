package org.marioarias.langur.parser

import org.marioarias.langur.ast.*
import org.marioarias.langur.lexer.Lexer
import org.marioarias.langur.token.{Token, TokenType}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
 * Created by IntelliJ IDEA.
 *
 * @author Mario Arias
 *         Date: 1/2/22
 *         Time: 1:50 PM
 */
class Parser(lexer: Lexer) {
  nextToken()
  nextToken()

  enum Precedence extends Ordered[Precedence] {
    case LOWEST, EQUALS, LESS_GREATER, SUM, PRODUCT, PREFIX, CALL, INDEX

    override def compare(that: Precedence): Int = this.ordinal.compare(that.ordinal)
  }

  private val innerErrors = ListBuffer.empty[String]
  private var curToken: Token = _
  private var peekToken: Token = _

  private val prefixParsers = Map[TokenType, () => Option[Expression]](
    TokenType.INT -> parseIntegerLiteral,
    TokenType.TRUE -> parseBooleanLiteral,
    TokenType.FALSE -> parseBooleanLiteral,
    TokenType.IDENT -> parseIdentifier,
    TokenType.BANG -> parsePrefixExpression,
    TokenType.MINUS -> parsePrefixExpression
  )
  private val infixParsers = Map[TokenType, Option[Expression] => Option[Expression]](
    TokenType.PLUS -> parseInfixExpression,
    TokenType.MINUS -> parseInfixExpression,
    TokenType.SLASH -> parseInfixExpression,
    TokenType.ASTERISK -> parseInfixExpression,
    TokenType.EQ -> parseInfixExpression,
    TokenType.NOT_EQ -> parseInfixExpression,
    TokenType.LT -> parseInfixExpression,
    TokenType.GT -> parseInfixExpression,
  )

  private val precedences = Map(
    TokenType.EQ -> Precedence.EQUALS,
    TokenType.NOT_EQ -> Precedence.EQUALS,
    TokenType.LT -> Precedence.LESS_GREATER,
    TokenType.GT -> Precedence.LESS_GREATER,
    TokenType.PLUS -> Precedence.SUM,
    TokenType.MINUS -> Precedence.SUM,
    TokenType.SLASH -> Precedence.PRODUCT,
    TokenType.ASTERISK -> Precedence.PRODUCT,
    TokenType.LPAREN -> Precedence.CALL,
    TokenType.LBRACKET -> Precedence.INDEX
  )

  def parseProgram(): Program = {
    val statements = ListBuffer.empty[Statement]
    while (curToken.tokenType != TokenType.EOF) {
      val statement = parseStatement()
      for st <- statement yield {
        statements += st
      }
      nextToken()
    }
    Program(statements.toList)
  }

  def errors(): List[String] = innerErrors.toList

  private def nextToken(): Unit = {
    try {
      curToken = peekToken
    } catch {
      case e: RuntimeException => // Ignore it
    }
    peekToken = lexer.nextToken()
  }

  private def parseStatement(): Option[Statement] = {
    curToken.tokenType match {
      case TokenType.LET => parseLetStatement()
      case TokenType.RETURN => Some(parseReturnStatement())
      case _ => Some(parseExpressionStatement())
    }
  }

  private def parseLetStatement(): Option[Statement] = {
    val token = curToken
    if (!expectPeek(TokenType.IDENT)) {
      return None
    }

    val name = Identifier(curToken, curToken.literal)

    if (!expectPeek(TokenType.ASSIGN)) {
      return None
    }

    nextToken()

    val value = parseExpression(Precedence.LOWEST)


    value match {
      case Some(literal: FunctionLiteral) => literal.name = name.value
      case _ =>
    }

    if (peekTokenIs(TokenType.SEMICOLON)) {
      nextToken()
    }

    Some(LetStatement(token, name, value))
  }


  private def parseExpression(precedence: Precedence): Option[Expression] = {
    val prefix = prefixParsers.get(curToken.tokenType)
    if (prefix.isEmpty) {
      noPrefixParserError(curToken.tokenType)
      return None
    }

    var left = prefix.get()

    while (!peekTokenIs(TokenType.SEMICOLON) && precedence < peekPrecedence()) {
      val infix = infixParsers.get(peekToken.tokenType)
      if (infix.isEmpty) {
        return left
      }
      nextToken()
      left = infix.get(left)
    }
    left
  }

  private def parseExpressionStatement(): Statement = {
    val token = curToken
    val expression = parseExpression(Precedence.LOWEST)
    if (peekTokenIs(TokenType.SEMICOLON)) {
      nextToken()
    }
    ExpressionStatement(token, expression)
  }

  private def parseReturnStatement(): Statement = {
    val token = curToken
    nextToken()

    val returnValue = parseExpression(Precedence.LOWEST)

    while (peekTokenIs(TokenType.SEMICOLON)) {
      nextToken()
    }

    ReturnStatement(token, returnValue)
  }

  private def parseIntegerLiteral(): Option[Expression] = {
    val token = curToken
    try {
      val value = token.literal.toLong
      Some(IntegerLiteral(token, value))
    } catch {
      case _: Throwable =>
        innerErrors += s"could not parse ${token.literal} as integer"
        None
    }
  }

  private def parseBooleanLiteral() = Some(BooleanLiteral(curToken, curTokenIs(TokenType.TRUE)))

  private def parseIdentifier() = Some(Identifier(curToken, curToken.literal))

  private def parsePrefixExpression() = {
    val token = curToken
    val operator = token.literal
    nextToken()
    val right = parseExpression(Precedence.PREFIX)
    Some(PrefixExpression(token, operator, right))
  }

  private def parseInfixExpression(left: Option[Expression]): Option[Expression] = {
    val token = curToken
    val operator = token.literal
    val precedence = curPrecedence()
    nextToken()
    val right = parseExpression(precedence)
    Some(InfixExpression(token, left, operator, right))
  }

  private def peekPrecedence(): Precedence = findPrecedence(peekToken.tokenType)

  private def curPrecedence(): Precedence = findPrecedence(curToken.tokenType)

  private def findPrecedence(tokenType: TokenType): Precedence = precedences.getOrElse(tokenType, Precedence.LOWEST)

  private def noPrefixParserError(tokenType: TokenType): Unit = innerErrors += s"no prefix parser for $tokenType type"

  private def peekError(tokenType: TokenType): Unit = innerErrors += s"Expected next token to be $tokenType, got ${peekToken.tokenType} instead"

  private def expectPeek(tokenType: TokenType): Boolean = if (peekTokenIs(tokenType)) {
    nextToken()
    true
  } else {
    peekError(tokenType)
    false
  }

  private def peekTokenIs(tokenType: TokenType): Boolean = peekToken.tokenType == tokenType

  private def curTokenIs(tokenType: TokenType) = curToken.tokenType == tokenType
}
