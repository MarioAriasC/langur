package org.marioarias.langur.ast

import org.marioarias.langur.token.Token

/** Created by IntelliJ IDEA.
  *
  * @author
  *   Mario Arias Date: 1/2/22 Time: 1:23 PM
  */
trait Node {
  def tokenLiteral(): String

  def toString: String
}

trait Statement extends Node

trait Expression extends Node

trait NodeAdapter {
  override def equals(obj: Any): Boolean = if (obj != null) {
    obj.toString == toString
  } else {
    false
  }

  override def hashCode(): Int = this.toString.hashCode
}

trait TokenHolder {
  val token: Token
}

trait ExpressionWithToken extends TokenHolder with Expression {
  override def tokenLiteral(): String = token.literal
}

trait StatementWithToken extends TokenHolder with Statement {
  override def tokenLiteral(): String = token.literal
}
