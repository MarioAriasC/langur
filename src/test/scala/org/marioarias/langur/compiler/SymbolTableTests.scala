package org.marioarias.langur.compiler

import org.marioarias.langur.fail
import utest.{ArrowAssert, TestSuite, Tests, test}

/**
 * Created by IntelliJ IDEA.
 *
 * @author Mario Arias
 *         Date: 1/3/22
 *         Time: 2:46 PM
 */
object SymbolTableTests extends TestSuite {
  override def tests: Tests = Tests {
    test("define") {
      val expected = Map(
        "a" -> Symbol("a", SymbolScope.GLOBAL, 0),
        "b" -> Symbol("b", SymbolScope.GLOBAL, 1),
        "c" -> Symbol("c", SymbolScope.LOCAL, 0),
        "d" -> Symbol("d", SymbolScope.LOCAL, 1),
        "e" -> Symbol("e", SymbolScope.LOCAL, 0),
        "f" -> Symbol("f", SymbolScope.LOCAL, 1),
      )

      def testSymbol(name: String, table: SymbolTable): Unit = {
        val symbol = table.define(name)
        val expectedSymbol = expected(name)
        expectedSymbol ==> symbol
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
    test("resolve global") {
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
    test("resolve local") {
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
        Symbol("d", SymbolScope.LOCAL, 1),
      ).foreach { sym =>
        testSymbol(local, sym)
      }
    }
    test("resolve nested local") {
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
          Symbol("d", SymbolScope.LOCAL, 1),
        ),
        secondLocal -> List(
          Symbol("a", SymbolScope.GLOBAL, 0),
          Symbol("b", SymbolScope.GLOBAL, 1),
          Symbol("e", SymbolScope.LOCAL, 0),
          Symbol("f", SymbolScope.LOCAL, 1),
        )
      ).foreach { case (table, symbols) =>
        symbols.foreach { symbol =>
          testSymbol(table, symbol)
        }
      }
    }
    test("define resolve builtins") {
      val global = SymbolTable()
      val firstLocal = SymbolTable(outer = Some(global))
      val secondLocal = SymbolTable(outer = Some(firstLocal))

      val expected = List(
        Symbol("a", SymbolScope.BUILTIN, 0),
        Symbol("c", SymbolScope.BUILTIN, 1),
        Symbol("e", SymbolScope.BUILTIN, 2),
        Symbol("f", SymbolScope.BUILTIN, 3),
      )

      expected.zipWithIndex.foreach { (symbol, i) =>
        global.defineBuiltin(i, symbol.name)
      }

      List(global, firstLocal, secondLocal).foreach { table =>
        expected.foreach { symbol =>
          try {
            val result = table.resolve(symbol.name)
            symbol ==> result
          } catch {
            case e: SymbolException => fail("name ${symbol.name} not resolvable")
          }
        }
      }
    }
    test("resolve free") {
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
            Symbol("d", SymbolScope.LOCAL, 1),
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
            Symbol("f", SymbolScope.LOCAL, 1),
          ),
          List(
            Symbol("c", SymbolScope.LOCAL, 0),
            Symbol("d", SymbolScope.LOCAL, 1),
          )
        )
      ).foreach {
        case (table, expectedSymbols, expectedFreeSymbols) =>
          expectedSymbols.foreach { sym =>
            testSymbol(table, sym)
          }

          table.freeSymbols.size ==> expectedFreeSymbols.length

          expectedFreeSymbols.zipWithIndex.foreach { (sym, i) =>

            val result = table.freeSymbols(i)
            sym ==> result
          }
      }
    }
    test("resolve unresolvable free") {
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
        Symbol("f", SymbolScope.LOCAL, 1),
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
    test("define and resolve function name") {
      val global = SymbolTable()
      global.defineFunctionName("a")

      val expected = Symbol("a", SymbolScope.FUNCTION, 0)
      testSymbol(global, expected)
    }
    test("shadowing function name") {
      val global = SymbolTable()
      global.defineFunctionName("a")
      global.define("a")

      val expected = Symbol("a", SymbolScope.GLOBAL, 0)
      testSymbol(global, expected)
    }
  }

  private def testSymbol(table: SymbolTable, sym: Symbol): Unit = {
    val result = table.resolve(sym.name)
    sym ==> result
  }
}
