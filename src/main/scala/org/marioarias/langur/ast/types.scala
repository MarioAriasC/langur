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
  override def toString: String = expression.map(_.toString).getOrElse("")
}

class ReturnStatement(override val token: Token, val returnValue: Option[Expression]) extends NodeAdapter with StatementWithToken {
  override def toString: String = s"${tokenLiteral()} ${returnValue.map(_.toString).getOrElse("")}"
}

class BlockStatement(override val token: Token, val statements: Option[List[Option[Statement]]]) extends NodeAdapter with StatementWithToken {
  override def toString: String = statements.getOrElse(List.empty).map(_.map(_.toString).getOrElse("")).mkString("")
}