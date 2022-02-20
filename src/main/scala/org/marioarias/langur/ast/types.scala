package org.marioarias.langur.ast

import org.marioarias.langur.token.Token

class Program(val statements: List[Statement]) extends NodeAdapter with Node {
  override def tokenLiteral(): String = if (statements.isEmpty) {
    ""
  } else {
    statements.head.tokenLiteral()
  }

  override def toString: String = statements.mkString("")
}


class Identifier(override val token: Token, val value: String) extends NodeAdapter with ExpressionWithToken {
  override def toString: String = value
}

class LetStatement(override val token: Token, val name: Identifier, val value: Option[Expression]) extends NodeAdapter with StatementWithToken {
  override def toString: String = s"${tokenLiteral()} $name = ${value.map(_.toString).getOrElse("")}"
}

trait LiteralExpression[T](override val token: Token, val value: T) extends NodeAdapter with ExpressionWithToken :
  override def toString: String = token.literal

class IntegerLiteral(token: Token, value: Long) extends LiteralExpression[Long](token, value)

class BooleanLiteral(token: Token, value: Boolean) extends LiteralExpression[Boolean](token, value)

class FunctionLiteral(override val token: Token, val parameters: Option[List[Identifier]], val body: Option[BlockStatement], var name: String = "") extends NodeAdapter with ExpressionWithToken {
  override def toString: String = s"${tokenLiteral()}${if (name.nonEmpty) s"<$name>" else ""}(${parameters.getOrElse(List.empty).mkString(", ")}) $body"
}

class ExpressionStatement(override val token: Token, val expression: Option[Expression]) extends NodeAdapter with StatementWithToken {
  override def toString: String = expression.debug()
}

class ReturnStatement(override val token: Token, val returnValue: Option[Expression]) extends NodeAdapter with StatementWithToken {
  override def toString: String = s"${tokenLiteral()} ${returnValue.map(_.toString).getOrElse("")}"
}

class BlockStatement(override val token: Token, val statements: Option[List[Option[Statement]]]) extends NodeAdapter with StatementWithToken {
  override def toString: String = statements.getOrElse(List.empty).map(_.map(_.toString).getOrElse("")).mkString("")
}

class PrefixExpression(override val token: Token, val operator: String, val right: Option[Expression]) extends NodeAdapter with ExpressionWithToken {
  override def toString: String = s"($operator${right.debug()})"
}

class InfixExpression(override val token: Token, val left: Option[Expression], val operator: String, val right: Option[Expression]) extends NodeAdapter with ExpressionWithToken {
  override def toString: String = s"(${left.debug()} $operator ${right.debug()})"
}

class CallExpression(override val token: Token, val function: Option[Expression], val arguments: Option[List[Option[Expression]]]) extends NodeAdapter with ExpressionWithToken {
  override def toString: String = s"${function.debug()}(${arguments.debugList()})"
}

class ArrayLiteral(override val token: Token, val elements: Option[List[Option[Expression]]]) extends NodeAdapter with ExpressionWithToken {
  override def toString: String = s"[${elements.debugList()}]"
}

class IndexExpression(override val token: Token, val left: Option[Expression], val index: Option[Expression]) extends NodeAdapter with ExpressionWithToken {
  override def toString: String = s"(${left.debug()}[${index.debug()}])"
}

class IfExpression(override val token: Token, val condition: Option[Expression], val consequence: Option[BlockStatement], val alternative: Option[BlockStatement]) extends NodeAdapter with ExpressionWithToken {
  override def toString: String = s"if ${condition.debug()} ${consequence.debug()} ${alternative.map(alt => s"else $alt").getOrElse("")}"
}

class StringLiteral(override val token: Token, val value: String) extends NodeAdapter with ExpressionWithToken {
  override def toString: String = value
}

class HashLiteral(override val token: Token, val pairs: Map[Expression, Expression]) extends NodeAdapter with ExpressionWithToken {
  override def toString: String = s"{${pairs.keys.map(key => s"$key:${pairs(key)}")}}"
}

extension[T] (exp: Option[T]) {
  def debug(): String = exp.map(_.toString).getOrElse("")
}

extension[T] (maybeList: Option[List[Option[T]]]) {
  def debugList(): String = maybeList.map { list =>
    list.map { maybe =>
      maybe.debug()
    }
  }.getOrElse(List.empty).mkString(", ")
}