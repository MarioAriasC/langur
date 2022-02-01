package org.marioarias.langur.lexer

import org.marioarias.langur.lexer.Lexer.{whiteSpaces, zeroChar}
import org.marioarias.langur.token._
import org.marioarias.langur.utils.Utils._

import scala.language.implicitConversions
import scala.util.control.Breaks.{break, breakable}

/**
 * Created by IntelliJ IDEA.
 *
 * @author Mario Arias
 *         Date: 31/1/22
 *         Time: 7:35 PM
 */
class Lexer(input: String) {
  private var position = 0
  private var readPosition = 0
  private var ch = zeroChar

  readChar()

  private def readChar(): Unit = {
    ch = peakChar()
    position = readPosition
    readPosition = readPosition + 1
  }

  private def peakChar(): Char = if (readPosition >= input.length) {
    zeroChar
  } else {
    input.charAt(readPosition)
  }

  private def readValue(predicate: Char => Boolean): String = {
    val currentPosition = position
    while (predicate(ch)) {
      readChar()
    }
    input.substring(currentPosition, position)
  }

  private def readNumber(): String = readValue(ch => ch.isDigit)

  private def readIdentifier(): String = readValue(ch => ch.isIdentifier)

  private def readString(): String = {
    val start = position + 1
    breakable {
      while (true) {
        readChar()
        if (ch == '"' || ch == 0.toChar) {
          break
        }
      }
    }
    input.substring(start, position)
  }

  def nextToken(): Token = {
    def endsWithEqual(oneChar: TokenType, twoChars: TokenType, duplicateChars: Boolean = true) = {
      if (peakChar() == '=') {
        val currentChar = ch
        readChar()
        val value = if (duplicateChars) {
          s"$currentChar$currentChar"
        } else {
          s"$currentChar$ch"
        }
        Token(twoChars, value)
      } else {
        oneChar.token()
      }
    }

    skipWhitespace()
    var readNextChar = true

    (ch match {
      case '=' => endsWithEqual(ASSIGN, EQ)
      case ';' => SEMICOLON.token()
      case ':' => COLON.token()
      case ',' => COMMA.token()
      case '(' => LPAREN.token()
      case ')' => RPAREN.token()
      case '{' => LBRACE.token()
      case '}' => RBRACE.token()
      case '[' => LBRACKET.token()
      case ']' => RBRACKET.token()
      case '+' => PLUS.token()
      case '-' => MINUS.token()
      case '*' => ASTERISK.token()
      case '/' => SLASH.token()
      case '<' => LT.token()
      case '>' => GT.token()
      case '!' => endsWithEqual(BANG, NOT_EQ, duplicateChars = false)
      case '"' => Token(STRING, readString())
      case Lexer.zeroChar => Token(EOF, "")
      case _ =>
        if (ch.isIdentifier) {
          val identifier = readIdentifier()
          readNextChar = false
          Token(identifier.lookupIdent(), identifier)
        } else if (ch.isDigit) {
          readNextChar = false
          Token(INT, readNumber())
        } else {
          Token(ILLEGAL, ch.toString)
        }
    }).also { _ =>
      if (readNextChar) {
        readChar()
      }
    }
  }

  private def skipWhitespace(): Unit = {
    while (whiteSpaces.contains(ch)) {
      readChar()
    }
  }

  implicit private class CharIdentifier(ch: Char) {
    def isIdentifier: Boolean = ch.isLetter || ch == '_'
  }

  implicit private class Tokens(tokenType: TokenType) {
    def token(): Token = Token(tokenType, ch.toString)
  }

  implicit private class Lookup(value: String) {
    def lookupIdent(): TokenType = {
      TokenType.keywords.get(value) match {
        case Some(tokenType) => tokenType
        case None => IDENT
      }
    }
  }


  override def toString = s"Lexer(input=$input position=$position, readPosition=$readPosition, ch=$ch)"
}

object Lexer {
  val whiteSpaces = List(' ', '\t', '\n', '\r')
  val zeroChar: Char = 0.toChar
}
