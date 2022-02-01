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