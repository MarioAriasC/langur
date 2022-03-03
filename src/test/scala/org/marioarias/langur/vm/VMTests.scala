package org.marioarias.langur.vm

import org.marioarias.langur.*
import org.marioarias.langur.compiler.MCompiler
import org.marioarias.langur.objects.*
import org.marioarias.langur.vm.VM.Null
import utest.{ArrowAssert, TestSuite, Tests, test}

/**
 * Created by IntelliJ IDEA.
 *
 * @author Mario Arias
 *         Date: 2/3/22
 *         Time: 8:08 AM
 */
object VMTests extends TestSuite {
  case class VTC[T](input: String, expected: T)

  override def tests: Tests = Tests {
    test("integer arithmetic") {
      List(
        VTC("1", 1),
        VTC("2", 2),
        VTC("1 + 2", 3),
        VTC("1 - 2", -1),
        VTC("1 * 2", 2),
        VTC("4 / 2", 2),
        VTC("50 / 2 * 2 + 10 - 5", 55),
        VTC("5 + 5 + 5 + 5 - 10", 10),
        VTC("2 * 2 * 2 * 2 * 2", 32),
        VTC("5 * 2 + 10", 20),
        VTC("5 + 2 * 10", 25),
        VTC("5 * (2 + 10)", 60),
        VTC("-5", -5),
        VTC("-10", -10),
        VTC("-50 + 100 + - 50", 0),
        VTC("(5 + 10 * 2 + 15  / 3) * 2 + - 10", 50),
      ).runVmTests()
    }
    test("boolean expressions") {
      List(
        VTC("true", true),
        VTC("false", false),
        VTC("1 < 2", true),
        VTC("1 > 2", false),
        VTC("1 > 1", false),
        VTC("1 == 1", true),
        VTC("1 != 1", false),
        VTC("1 == 2", false),
        VTC("1 != 2", true),
        VTC("true == true", true),
        VTC("false == false", true),
        VTC("true == false", false),
        VTC("true != false", true),
        VTC("false != true", true),
        VTC("(1 < 2) == true", true),
        VTC("(1 < 2) == false", false),
        VTC("(1 > 2) == true", false),
        VTC("(1 > 2) == false", true),
        VTC("!true", false),
        VTC("!false", true),
        VTC("!5", false),
        VTC("!!true", true),
        VTC("!!false", false),
        VTC("!!5", true),
        VTC("!(if (false) { 5; })", true),
      ).runVmTests()
    }
    test("conditional") {
      List(
        VTC("if (true) {10}", 10),
        VTC("if (true) {10} else {20}", 10),
        VTC("if (false) {10} else {20}", 20),
        VTC("if (1) {10}", 10),
        VTC("if (1 < 2) {10}", 10),
        VTC("if (1 < 2) {10} else {20}", 10),
        VTC("if (1 > 2) {10} else {20}", 20),
        VTC("if (1 > 2) {10}", Null),
        VTC("if (false) {10}", Null),
        VTC("if ((if (false) {10})) {10} else {20}", 20),
      ).runVmTests()
    }
    test("global let statement") {
      List(
        VTC("let one = 1; one;", 1),
        VTC("let one = 1; let two = 2; one + two", 3),
        VTC("let one = 1; let two = one + one; one + two", 3),
      ).runVmTests()
    }
    test("string expressions") {
      List(
        VTC(""" "monkey" """, "monkey"),
        VTC(""" "mon" + "key" """, "monkey"),
        VTC(""" "mon" + "key" + "banana" """, "monkeybanana"),
      ).runVmTests()
    }
    test("array literals") {
      List(
        VTC("[]", List.empty),
        VTC("[1, 2, 3]", List(1L, 2L, 3L)),
        VTC("[1 + 2, 3 * 4, 5 + 6]", List(3L, 12L, 11L)),
      ).runVmTests()
    }
    test("hash literals") {
      List(
        VTC("{}", Map.empty),
        VTC(
          "{1: 2, 2: 3}", Map(
            MInteger(1).hashKey() -> 2L,
            MInteger(2).hashKey() -> 3L
          )
        ),
        VTC(
          "{1 + 1: 2 * 2, 3 + 3: 4 * 4}", Map(
            MInteger(2).hashKey() -> 4L,
            MInteger(6).hashKey() -> 16L
          )
        ),
      ).runVmTests()
    }
    test("index expressions") {
      List(
        VTC("[1, 2, 3][1]", 2),
        VTC("[1, 2, 3][0 + 2]", 3),
        VTC("[[1, 1, 1]][0][0]", 1),
        VTC("[][0]", Null),
        VTC("[1, 2, 3][99]", Null),
        VTC("[1][-1]", Null),
        VTC("{1: 1, 2: 2}[1]", 1),
        VTC("{1: 1, 2: 2}[2]", 2),
        VTC("{1: 1}[0]", Null),
        VTC("{}[0]", Null),
      ).runVmTests()
    }
    test("calling functions without arguments") {
      List(
        VTC(
          """
               let fivePlusTen = fn() {5 + 10; };
               fivePlusTen()
               """, 15
        ),
        VTC(
          """
               let one = fn() { 1; }
               let two = fn() { 2; }
               one() + two()
               """, 3
        ),
        VTC(
          """
              let a = fn() { 1 };
              let b = fn () { a() + 1 };
              let c = fn () { b() + 1 };
              c();
              """,
          3,
        ),
      ).runVmTests()
    }
    test("functions with return statement") {
      List(
        VTC(
          """
                  let earlyExit = fn() { return 99; 100; };
                  earlyExit();
                  """,
          99,
        ),
        VTC(
          """
                  let earlyExit = fn() { return 99; return 100; };
                  earlyExit();
                  """,
          99,
        )
      ).runVmTests()
    }
    test("functions without return value") {
      List(
        VTC(
          """
                  let noReturn = fn() {};
                  noReturn();
                  """,
          Null,
        ),
        VTC(
          """
                  let noReturn = fn() {};
                  let noReturnTwo = fn() { noReturn(); };
                  noReturn();
                  noReturnTwo();
                  """,
          Null,
        ),
      ).runVmTests()
    }
    test("first class functions") {
      List(
        VTC(
          """
                  let returnsOne = fn(){ 1;};
                  let returnsOneReturner = fn() {returnsOne;}
                  returnsOneReturner()();
                  """,
          1,
        ),
        VTC(
          """
                  let returnsOneReturner = fn() {
                  	let returnsOne = fn() {
                  		1;
                  	};
                  	returnsOne;
                  }
                  returnsOneReturner()();
                  """,

          1,
        ),
      ).runVmTests()
    }
    test("calling functions with bindings") {
      List(
        VTC(
          """
                  let one = fn() { let one = 1; one;};
                  one();
                  """,
          1,
        ),
        VTC(
          """
                  let oneAndTwo = fn() {
                  	let one = 1;
                  	let two = 2;
                  	one + two;
                  };
                  oneAndTwo();
                  """,
          3,
        ),
        VTC(
          """
                  let oneAndTwo = fn() {
                  	let one = 1;
                  	let two = 2;
                  	one + two;
                  };
                  let threeAndFour = fn() {
                  	let three = 3;
                  	let four = 4;
                  	three + four;
                  };
                  oneAndTwo() + threeAndFour();
                  """,
          10,
        ),
        VTC(
          """
                  let firstFoobar = fn() {
                  	let foobar = 50;
                  	foobar;
                  };

                  let secondFoobar = fn() {
                  	let foobar = 100;
                  	foobar;
                  };
                  firstFoobar() + secondFoobar();
                  """,
          150,
        ),
        VTC(
          """
                  let globalSeed = 50;
                  let minusOne = fn() {
                  	let num = 1;
                  	globalSeed - num;
                  };
                  let minusTwo = fn() {
                  	let num = 2;
                  	globalSeed - num;
                  };
                  minusOne() + minusTwo();
                  """,
          97,
        ),
      ).runVmTests()
    }
    test("calling functions with arguments and bindings") {
      List(
        VTC(
          """
                  let identity = fn(a) { a; };
                  identity(4);
                  """,
          4,
        ),
        VTC(
          """
                  let sum = fn(a, b) { a + b; };
                  sum(1, 2);
                  """,
          3,
        ),
        VTC(
          """
                  let sum = fn(a, b){
                  	let c = a + b;
                  	c;
                  }
                  sum(1, 2);
                  """,
          3,
        ),
        VTC(
          """
                  let sum = fn(a, b){
                  	let c = a + b;
                  	c;
                  }
                  sum(1, 2) + sum(3, 4);
                  """,
          10,
        ),
        VTC(
          """
                  let sum = fn(a, b){
                  	let c = a + b;
                  	c;
                  }
                  let outer = fn() {
                  	sum(1, 2) + sum(3, 4);
                  };
                  outer();
                  """,
          10,
        ),
        VTC(
          """
                  let globalNum = 10;
                  let sum = fn(a, b){
                  	let c = a + b;
                  	c + globalNum;
                  }
                  let outer = fn() {
                  	sum(1, 2) + sum(3, 4) + globalNum;
                  };
                  outer() + globalNum;
                  """,
          50,
        ),
      ).runVmTests()
    }
    test("calling functions with wrong arguments") {
      List(
        VTC(
          "fn() {1;}(1);",
          "wrong number of arguments: want=0, got=1",
        ),
        VTC(
          "fn(a) {a;}();",
          "wrong number of arguments: want=1, got=0",
        ),
        VTC(
          "fn(a, b) {a + b;}(1);",
          "wrong number of arguments: want=2, got=1",
        ),
      ).foreach { case VTC(input, expected) =>
        val program = parse(input)
        val compiler = MCompiler()
        compiler.compile(program)

        val vm = VM(compiler.bytecode)

        try {
          vm.run()
          fail("expected VM error but resulted in none.")
        } catch {
          case e: VMException => expected ==> e.getMessage
        }
      }
    }
    test("builtins functions") {
      List(
        VTC("len(\"\")", 0),
        VTC("len(\"four\")", 4),
        VTC("len(\"hello world\")", 11),
        VTC(
          "len(1)", MError(
            "argument to `len` not supported, got MInteger",
          )
        ),
        VTC(
          "len(\"one\", \"two\")", MError(
            "wrong number of arguments. got=2, want=1"
          )
        ),
        VTC("len([1, 2, 3])", 3),
        VTC("len([])", 0),
        VTC("puts(\"hello\", \"world!\")", Null),
        VTC("first([1, 2, 3])", 1),
        VTC("first([])", Null),
        VTC(
          "first(1)", MError(
            "argument to `first` must be ARRAY, got MInteger"
          )
        ),
        VTC("last([1, 2, 3])", 3),
        VTC("last([])", Null),
        VTC(
          "last(1)", MError(
            "argument to `last` must be ARRAY, got MInteger"
          )
        ),
        VTC("rest([1,2,3])", List(2L, 3L)),
        VTC("rest([])", Null),
        VTC("push([], 1)", List(1L)),
        VTC(
          "push(1, 1)", MError(
            "argument to `push` must be ARRAY, got MInteger"
          )
        ),
      ).runVmTests()
    }
    test("closures") {
      List(
        VTC(
          """
                  let newClosure = fn(a) {
                  	fn() {a; };
                  };
                  let closure = newClosure(99);
                  closure();
                  """,
          99,
        ),
        VTC(
          """
                      let newAdder = fn (a, b) {
                      fn(c) { a + b + c };
                  };
                      let adder = newAdder (1, 2);
                      adder(8);
                      """,
          11,
        ),
        VTC(
          """
                      let newAdder = fn (a, b) {
                      let c = a +b;
                      fn(d) { c + d };
                  };
                      let adder = newAdder (1, 2);
                      adder(8);
                      """,
          11,
        ),
        VTC(
          """
                      let newAdderOuter = fn (a, b) {
                      let c = a +b;
                      fn(d) {
                          let e = d +c;
                          fn(f) { e + f; };
                      };
                  };
                      let newAdderInner = newAdderOuter (1, 2);
                      let adder = newAdderInner (3);
                      adder(8);
                      """,
          14,
        ),
        VTC(
          """
                      let a = 1;
                      let newAdderOuter = fn (b) {
                          fn(c) {
                              fn(d) { a + b + c + d };
                          };
                      };
                      let newAdderInner = newAdderOuter (2);
                      let adder = newAdderInner (3);
                      adder(8);
                      """,
          14,
        ),
        VTC(
          """
                      let newClosure = fn (a, b) {
                      let one = fn () { a; };
                      let two = fn () { b; };
                      fn() { one() + two(); };
                  };
                      let closure = newClosure (9, 90);
                      closure();
                      """,
          99,
        )
      ).runVmTests()
    }
    test("recursive functions") {
      List(
        VTC(
          """
                  let countDown = fn(x) {
                  	if (x == 0) {
                  		return 0;
                  	} else {
                  		countDown(x - 1);
                  	};
                  }
                  countDown(1);
                  """,
          0,
        ),
        VTC(
          """
                  let countDown = fn(x) {
                  	if (x == 0) {
                  		return 0;
                  	} else {
                  		countDown(x - 1);
                  	};
                  }
                  let wrapper = fn() {
                  	countDown(1);
                  };
                  wrapper();
                  """,
          0,
        ),
        VTC(
          """
                  let wrapper = fn() {
                  	let countDown = fn(x) {
                  		if (x == 0) {
                  			return 0;
                  		} else {
                  			countDown(x - 1);
                  		};
                  	};
                  	countDown(1);
                  };
                  wrapper();
                  """,
          0
        )
      ).runVmTests()
    }
    test("recursive fibonacci") {
      List(
        VTC(
          """
      let fibonacci = fn(x) {
      	if (x == 0) {
      		return 0;
      	} else {
      		if (x == 1) {
      			return 1;
      		} else {
      			fibonacci(x - 1) + fibonacci(x - 2);
      		}
      	}
      };
      fibonacci(15);

              """.stripMargin, 610
        )
      ).runVmTests()
    }
  }

  extension[T] (tests: List[VTC[T]]) {
    def runVmTests(): Unit = {
      tests.foreach { case VTC(input, expected) =>
        println(input)
        val program = parse(input)
        val compiler = MCompiler()
        compiler.compile(program)
        val vm = VM(compiler.bytecode)
        vm.run()
        val stackElm = vm.lastPoppedStackElem.get
        testExpectedObject(expected, stackElm)
      }
    }
  }

  private def testExpectedObject[T](expected: T, actual: MObject): Unit = {
    expected match {
      case i: Int => testIntegerObject(i, actual)
      case b: Boolean => testBooleanObject(b, actual)
      case MNull => MNull ==> actual
      case s: String => testStringObject(s, actual)
      case l: List[?] =>
        checkType(actual) { (array: MArray) =>
          array.elements.length ==> l.length
          l.zipWithIndex.foreach { (any, i) =>
            testIntegerObject(any.asInstanceOf[Long], array.elements(i).get)
          }
        }
      case m: Map[?, ?] =>
        checkType(actual) { (hash: MHash) =>
          m.size ==> hash.pairs.size
          m.foreach { case (expectedKey, expectedValue) =>
            val pair = hash.pairs(expectedKey.asInstanceOf[HashKey])
            (pair == null) ==> false
            testIntegerObject(expectedValue.asInstanceOf[Long], pair.value)
          }
        }
      case e: MError => checkType(actual) { (error: MError) =>
        error.message ==> e.message
      }
    }
  }

  private def testBooleanObject(expected: Boolean, actual: MObject): Unit = {
    expected ==> actual.asInstanceOf[MBoolean].value
  }
}
