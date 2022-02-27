package org.marioarias.langur.compiler

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
 * Created by IntelliJ IDEA.
 *
 * @author Mario Arias
 *         Date: 27/2/22
 *         Time: 1:35 PM
 */
case class Symbol(name: String, scope: SymbolScope, index: Int)

enum SymbolScope {
  case GLOBAL, LOCAL, BUILTIN, FREE, FUNCTION
}

class SymbolTable(private val store: mutable.HashMap[String, Symbol] = mutable.HashMap.empty,
                  val outer: Option[SymbolTable] = None) {
  var numDefinitions = 0
  val freeSymbols: mutable.ListBuffer[Symbol] = mutable.ListBuffer.empty[Symbol]

  def define(name: String): Symbol = {
    val scope = if (outer.isEmpty) {
      SymbolScope.GLOBAL
    } else {
      SymbolScope.LOCAL
    }
    val symbol = Symbol(name, scope, numDefinitions)
    store(name) = symbol
    numDefinitions = numDefinitions + 1
    symbol
  }

  def resolve(name: String): Symbol = {
    store.get(name) match {
      case Some(s: Symbol) => s
      case None =>
        outer match {
          case Some(out: SymbolTable) =>
            val symbol = out.resolve(name)
            if (symbol.scope == SymbolScope.GLOBAL || symbol.scope == SymbolScope.BUILTIN) {
              symbol
            } else {
              defineFree(symbol)
            }
          case None => throw SymbolException(s"undefined variable $name")
        }
    }
  }

  private def defineFree(original: Symbol): Symbol = {
    freeSymbols += original
    val symbol = Symbol(original.name, SymbolScope.FREE, freeSymbols.length - 1)
    store(original.name) = symbol
    symbol
  }
}

class SymbolException(message: String) extends Exception(message)
