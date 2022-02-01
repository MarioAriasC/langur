package org.marioarias.langur.token

/**
 * Created by IntelliJ IDEA.
 *
 * @author Mario Arias
 *         Date: 31/1/22
 *         Time: 8:21 PM
 */
sealed trait TokenType {
  val value: String
}

case object ILLEGAL extends TokenType {
  override val value = "ILLEGAL"
}

case object EOF extends TokenType {
  override val value = "EOF"
}

case object ASSIGN extends TokenType {
  override val value = "="
}

case object EQ extends TokenType {
  override val value = "=="
}

case object NOT_EQ extends TokenType {
  override val value = "!="
}

case object IDENT extends TokenType {
  override val value = "IDENT"
}

case object INT extends TokenType {
  override val value = "INT"
}

case object PLUS extends TokenType {
  override val value = "+"
}

case object COMMA extends TokenType {
  override val value = ","
}

case object SEMICOLON extends TokenType {
  override val value = ";"
}

case object COLON extends TokenType {
  override val value = ":"
}

case object MINUS extends TokenType {
  override val value = "-"
}

case object BANG extends TokenType {
  override val value = "!"
}

case object SLASH extends TokenType {
  override val value = "/"
}

case object ASTERISK extends TokenType {
  override val value = "*"
}

case object LT extends TokenType {
  override val value = "<"
}

case object GT extends TokenType {
  override val value = ">"
}

case object LPAREN extends TokenType {
  override val value = "("
}

case object RPAREN extends TokenType {
  override val value = ")"
}

case object LBRACE extends TokenType {
  override val value = "{"
}

case object RBRACE extends TokenType {
  override val value = "}"
}

case object LBRACKET extends TokenType {
  override val value = "["
}

case object RBRACKET extends TokenType {
  override val value = "]"
}

case object FUNCTION extends TokenType {
  override val value = "FUNCTION"
}

case object LET extends TokenType {
  override val value = "LET"
}

case object TRUE extends TokenType {
  override val value = "TRUE"
}

case object FALSE extends TokenType {
  override val value = "FALSE"
}

case object IF extends TokenType {
  override val value = "IF"
}

case object ELSE extends TokenType {
  override val value = "ELSE"
}

case object RETURN extends TokenType {
  override val value = "RETURN"
}

case object STRING extends TokenType {
  override val value = "STRING"
}


object TokenType {
  val keywords = Map(
    "fn" -> FUNCTION,
    "let" -> LET,
    "true" -> TRUE,
    "false" -> FALSE,
    "if" -> IF,
    "else" -> ELSE,
    "return" -> RETURN
  )
}


case class Token(tokenType: TokenType, literal: String)
