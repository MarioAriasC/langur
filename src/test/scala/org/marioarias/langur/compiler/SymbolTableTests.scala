package org.marioarias.langur.compiler

import org.junit.Assert.assertTrue
import org.junit.Test
import org.marioarias.langur.fail

/** Created by IntelliJ IDEA.
  *
  * @author
  *   Mario Arias Date: 1/3/22 Time: 2:46 PM
  */
class SymbolTableTests {

  @Test def `define`(): Unit = {
    val expected = Map(
      "a" -> Symbol("a", SymbolScope.GLOBAL, 0),
      "b" -> Symbol("b", SymbolScope.GLOBAL, 1),
      "c" -> Symbol("c", SymbolScope.LOCAL, 0),
      "d" -> Symbol("d", SymbolScope.LOCAL, 1),
      "e" -> Symbol("e", SymbolScope.LOCAL, 0),
      "f" -> Symbol("f", SymbolScope.LOCAL, 1)
    )

    def testSymbol(name: String, table: SymbolTable): Unit = {
      val symbol = table.define(name)
      val expectedSymbol = expected(name)
      assertTrue(expectedSymbol == symbol)
    }

    val global = SymbolTable()

    testSymbol("a", global)
    testSymbol("b", global)

    val firstLocal = SymbolTable(outer = Some(global))

    testSymbol("c", firstLocal)
    testSymbol("d", firstLocal)

    val secondLocal = SymbolTable(outer = Some(global))

    testSymbol("e", secondLocal)
    testSymbol("f", secondLocal)
  }
  @Test def `resolve global`(): Unit = {
    val global = SymbolTable()
    global.define("a")
    global.define("b")

    val expected = Map(
      "a" -> Symbol("a", SymbolScope.GLOBAL, 0),
      "b" -> Symbol("b", SymbolScope.GLOBAL, 1)
    )

    expected.values.foreach { symbol =>
      try {
        testSymbol(global, symbol)
      } catch {
        case e: SymbolException => fail("name ${symbol.name} not resolvable")
      }
    }
  }
  @Test def `resolve local`(): Unit = {
    val global = SymbolTable()
    global.define("a")
    global.define("b")

    val local = SymbolTable(outer = Some(global))
    local.define("c")
    local.define("d")

    List(
      Symbol("a", SymbolScope.GLOBAL, 0),
      Symbol("b", SymbolScope.GLOBAL, 1),
      Symbol("c", SymbolScope.LOCAL, 0),
      Symbol("d", SymbolScope.LOCAL, 1)
    ).foreach { sym =>
      testSymbol(local, sym)
    }
  }
  @Test def `resolve nested local`(): Unit = {
    val global = SymbolTable()
    global.define("a")
    global.define("b")

    val firstLocal = SymbolTable(outer = Some(global))
    firstLocal.define("c")
    firstLocal.define("d")

    val secondLocal = SymbolTable(outer = Some(global))
    secondLocal.define("e")
    secondLocal.define("f")

    List(
      firstLocal -> List(
        Symbol("a", SymbolScope.GLOBAL, 0),
        Symbol("b", SymbolScope.GLOBAL, 1),
        Symbol("c", SymbolScope.LOCAL, 0),
        Symbol("d", SymbolScope.LOCAL, 1)
      ),
      secondLocal -> List(
        Symbol("a", SymbolScope.GLOBAL, 0),
        Symbol("b", SymbolScope.GLOBAL, 1),
        Symbol("e", SymbolScope.LOCAL, 0),
        Symbol("f", SymbolScope.LOCAL, 1)
      )
    ).foreach { case (table, symbols) =>
      symbols.foreach { symbol =>
        testSymbol(table, symbol)
      }
    }
  }
  @Test def `define resolve builtins`(): Unit = {
    val global = SymbolTable()
    val firstLocal = SymbolTable(outer = Some(global))
    val secondLocal = SymbolTable(outer = Some(firstLocal))

    val expected = List(
      Symbol("a", SymbolScope.BUILTIN, 0),
      Symbol("c", SymbolScope.BUILTIN, 1),
      Symbol("e", SymbolScope.BUILTIN, 2),
      Symbol("f", SymbolScope.BUILTIN, 3)
    )

    expected.zipWithIndex.foreach { (symbol, i) =>
      global.defineBuiltin(i, symbol.name)
    }

    List(global, firstLocal, secondLocal).foreach { table =>
      expected.foreach { symbol =>
        try {
          val result = table.resolve(symbol.name)
          assertTrue(symbol == result)
        } catch {
          case e: SymbolException =>
            fail("name ${symbol.name} not resolvable")
        }
      }
    }
  }
  @Test def `resolve free`(): Unit = {
    val global = SymbolTable()
    global.define("a")
    global.define("b")

    val firstLocal = SymbolTable(outer = Some(global))
    firstLocal.define("c")
    firstLocal.define("d")

    val secondLocal = SymbolTable(outer = Some(firstLocal))
    secondLocal.define("e")
    secondLocal.define("f")

    List(
      (
        firstLocal,
        List(
          Symbol("a", SymbolScope.GLOBAL, 0),
          Symbol("b", SymbolScope.GLOBAL, 1),
          Symbol("c", SymbolScope.LOCAL, 0),
          Symbol("d", SymbolScope.LOCAL, 1)
        ),
        List.empty
      ),
      (
        secondLocal,
        List(
          Symbol("a", SymbolScope.GLOBAL, 0),
          Symbol("b", SymbolScope.GLOBAL, 1),
          Symbol("c", SymbolScope.FREE, 0),
          Symbol("d", SymbolScope.FREE, 1),
          Symbol("e", SymbolScope.LOCAL, 0),
          Symbol("f", SymbolScope.LOCAL, 1)
        ),
        List(
          Symbol("c", SymbolScope.LOCAL, 0),
          Symbol("d", SymbolScope.LOCAL, 1)
        )
      )
    ).foreach { case (table, expectedSymbols, expectedFreeSymbols) =>
      expectedSymbols.foreach { sym =>
        testSymbol(table, sym)
      }

      assertTrue(table.freeSymbols.size == expectedFreeSymbols.length)

      expectedFreeSymbols.zipWithIndex.foreach { (sym, i) =>

        val result = table.freeSymbols(i)
        assertTrue(sym == result)
      }
    }
  }
  @Test def `resolve unresolvable free`(): Unit = {
    val global = SymbolTable()
    global.define("a")

    val firstLocal = SymbolTable(outer = Some(global))
    firstLocal.define("c")

    val secondLocal = SymbolTable(outer = Some(firstLocal))
    secondLocal.define("e")
    secondLocal.define("f")

    List(
      Symbol("a", SymbolScope.GLOBAL, 0),
      Symbol("c", SymbolScope.FREE, 0),
      Symbol("e", SymbolScope.LOCAL, 0),
      Symbol("f", SymbolScope.LOCAL, 1)
    ).foreach { expected =>
      testSymbol(secondLocal, expected)
    }

    List("b", "d").foreach { unresolvable =>
      try {
        secondLocal.resolve(unresolvable)
        fail("Name $unresolvable resolved, but was expected not to")
      } catch {
        case e: SymbolException => // OK
      }
    }
  }
  @Test def `define and resolve function name`(): Unit = {
    val global = SymbolTable()
    global.defineFunctionName("a")

    val expected = Symbol("a", SymbolScope.FUNCTION, 0)
    testSymbol(global, expected)
  }
  @Test def `shadowing function name`(): Unit = {
    val global = SymbolTable()
    global.defineFunctionName("a")
    global.define("a")

    val expected = Symbol("a", SymbolScope.GLOBAL, 0)
    testSymbol(global, expected)
  }
}

private def testSymbol(table: SymbolTable, sym: Symbol): Unit = {
  val result = table.resolve(sym.name)
  assertTrue(sym == result)
}
