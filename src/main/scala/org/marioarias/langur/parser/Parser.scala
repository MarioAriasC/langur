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
    TokenType.MINUS -> parsePrefixExpression,
    TokenType.LPAREN -> parseGroupExpression,
    TokenType.LBRACKET -> parseArrayLiteral,
    TokenType.IF -> parseIfExpression,
    TokenType.FUNCTION -> parseFunctionLiteral,
    TokenType.STRING -> parseStringLiteral,
    TokenType.LBRACE -> parseHashLiteral
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
    TokenType.LPAREN -> parseCallExpression,
    TokenType.LBRACKET -> parseIndexExpression
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

  private def parseGroupExpression(): Option[Expression] = {
    nextToken()
    val exp = parseExpression(Precedence.LOWEST)

    if (!expectPeek(TokenType.RPAREN)) {
      None
    } else {
      exp
    }
  }

  private def parseCallExpression(expression: Option[Expression]): Option[Expression] = {
    val token = curToken
    val arguments = parseExpressionList(TokenType.RPAREN)
    Some(CallExpression(token, expression, arguments))
  }

  private def parseExpressionList(end: TokenType): Option[List[Option[Expression]]] = {
    val arguments = mutable.ListBuffer.empty[Option[Expression]]

    if (peekTokenIs(end)) {
      nextToken()
      return Some(arguments.toList)
    }

    nextToken()
    arguments += parseExpression(Precedence.LOWEST)

    while (peekTokenIs(TokenType.COMMA)) {
      nextToken()
      nextToken()
      arguments += parseExpression(Precedence.LOWEST)
    }

    if (!expectPeek(end)) {
      None
    } else {
      Some(arguments.toList)
    }
  }

  private def parseArrayLiteral(): Option[Expression] = {
    val token = curToken
    Some(ArrayLiteral(token, parseExpressionList(TokenType.RBRACKET)))
  }

  private def parseIndexExpression(left: Option[Expression]): Option[Expression] = {
    val token = curToken
    nextToken()

    val index = parseExpression(Precedence.LOWEST)

    if (!expectPeek(TokenType.RBRACKET)) {
      None
    } else {
      Some(IndexExpression(token, left, index))
    }
  }

  private def parseIfExpression(): Option[Expression] = {
    val token = curToken

    if (!expectPeek(TokenType.LPAREN)) {
      return None
    }

    nextToken()

    val condition = parseExpression(Precedence.LOWEST)

    if (!expectPeek(TokenType.RPAREN)) {
      return None
    }

    if (!expectPeek(TokenType.LBRACE)) {
      return None
    }

    val consequence = parseBlockStatement()

    val alternative = if (peekTokenIs(TokenType.ELSE)) {
      nextToken()

      if (!expectPeek(TokenType.LBRACE)) {
        return None
      }
      Some(parseBlockStatement())
    } else {
      None
    }

    Some(IfExpression(token, condition, Some(consequence), alternative))
  }

  private def parseBlockStatement(): BlockStatement = {
    val token = curToken

    val statements = mutable.ListBuffer.empty[Option[Statement]]

    nextToken()

    while (!curTokenIs(TokenType.RBRACE) && !curTokenIs(TokenType.EOF)) {
      val statement = parseStatement()
      if (statement.isDefined) {
        statements += statement
      }
      nextToken()
    }

    BlockStatement(token, Some(statements.toList))
  }

  private def parseFunctionLiteral(): Option[Expression] = {
    val token = curToken
    if (!expectPeek(TokenType.LPAREN)) {
      return None
    }

    val parameters = parseFunctionParameters()

    if (!expectPeek(TokenType.LBRACE)) {
      return None
    }

    val body = parseBlockStatement()

    Some(FunctionLiteral(token, parameters, Some(body)))
  }

  private def parseFunctionParameters(): Option[List[Identifier]] = {
    val parameters = mutable.ListBuffer.empty[Identifier]

    if (peekTokenIs(TokenType.RPAREN)) {
      nextToken()
      return Some(parameters.toList)
    }
    nextToken()

    val token = curToken

    parameters += Identifier(token, token.literal)

    while (peekTokenIs(TokenType.COMMA)) {
      nextToken()
      nextToken()

      val innerToken = curToken

      parameters += Identifier(innerToken, innerToken.literal)
    }

    if (!expectPeek(TokenType.RPAREN)) {
      return None
    }

    Some(parameters.toList)
  }

  private def parseStringLiteral() = Some(StringLiteral(curToken, curToken.literal))

  private def parseHashLiteral(): Option[Expression] = {
    val token = curToken
    val pairs = mutable.HashMap.empty[Expression, Expression]
    while (!peekTokenIs(TokenType.RBRACE)) {
      nextToken()
      val maybeKey = parseExpression(Precedence.LOWEST)
      if (!expectPeek(TokenType.COLON)) {
        return None
      }
      nextToken()
      val maybeValue = parseExpression(Precedence.LOWEST)
      for key <- maybeKey yield {
        for value <- maybeValue yield {
          pairs(key) = value
        }
      }
      if (!peekTokenIs(TokenType.RBRACE) && !expectPeek(TokenType.COMMA)) {
        return None
      }
    }
    if (!expectPeek(TokenType.RBRACE)) {
      None
    } else {
      Some(HashLiteral(token, pairs.toMap))
    }
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
