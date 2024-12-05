package org.marioarias.langur.token

/** Created by IntelliJ IDEA.
  *
  * @author
  *   Mario Arias Date: 31/1/22 Time: 8:21 PM
  */
enum TokenType(val value: String) {
  case ILLEGAL extends TokenType("ILLEGAL")
  case EOF extends TokenType("EOF")
  case ASSIGN extends TokenType("=")
  case EQ extends TokenType("==")
  case NOT_EQ extends TokenType("!=")
  case IDENT extends TokenType("IDENT")
  case INT extends TokenType("INT")
  case PLUS extends TokenType("+")
  case COMMA extends TokenType(",")
  case SEMICOLON extends TokenType(";")
  case COLON extends TokenType(":")
  case MINUS extends TokenType("-")
  case BANG extends TokenType("!")
  case SLASH extends TokenType("/")
  case ASTERISK extends TokenType("*")
  case LT extends TokenType("<")
  case GT extends TokenType(">")
  case LPAREN extends TokenType("(")
  case RPAREN extends TokenType(")")
  case LBRACE extends TokenType("{")
  case RBRACE extends TokenType("}")
  case LBRACKET extends TokenType("[")
  case RBRACKET extends TokenType("]")
  case FUNCTION extends TokenType("FUNCTION")
  case LET extends TokenType("LET")
  case TRUE extends TokenType("TRUE")
  case FALSE extends TokenType("FALSE")
  case IF extends TokenType("IF")
  case ELSE extends TokenType("ELSE")
  case RETURN extends TokenType("RETURN")
  case STRING extends TokenType("STRING")
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
