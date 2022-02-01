package org.marioarias.langur.parser

import org.marioarias.langur.ast.Program
import org.marioarias.langur.lexer.Lexer

/**
 * Created by IntelliJ IDEA.
 *
 * @author Mario Arias
 *         Date: 1/2/22
 *         Time: 1:50 PM
 */
class Parser(lexer: Lexer) {
  def parseProgram(): Program = ???
  def errors(): List[String] = ???
}
