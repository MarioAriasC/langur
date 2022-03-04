package org.marioarias.langur.benchmarks

import org.marioarias.langur.ast.Program
import org.marioarias.langur.compiler.MCompiler
import org.marioarias.langur.evaluator.{Environment, Evaluator}
import org.marioarias.langur.lexer.Lexer
import org.marioarias.langur.objects.MObject
import org.marioarias.langur.parser.Parser
import org.marioarias.langur.vm.VM

/**
 * Created by IntelliJ IDEA.
 *
 * @author Mario Arias
 *         Date: 4/3/22
 *         Time: 8:30 AM
 */
object Benchmarks {
  private val slowInput =
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
  fibonacci(35);
      """

  private val fastInput =
    """
  let fibonacci = fn(x) {
      if (x < 2) {
      	return x;
      } else {
      	fibonacci(x - 1) + fibonacci(x - 2);
      }
  };
  fibonacci(35);
      """

  private def measure(engine: String)(body: => MObject): Unit = {
    val start = System.currentTimeMillis()
    val result = body
    val end = System.currentTimeMillis()
    println(s"engine=$engine, result=${result.inspect()}, duration=${end - start}ms")
  }

  private def parse(input: String): Program = {
    val lexer = Lexer(input)
    val parser = Parser(lexer)
    parser.parseProgram()
  }

  private def eval(engine: String, input: String = slowInput): Unit = {
    val environment = Environment.newEnvironment()
    measure(engine) {
      Evaluator.eval(Some(parse(input)), environment).get
    }
  }

  private def vm(engine: String, input: String = slowInput): Unit = {
    val compiler = MCompiler()
    compiler.compile(parse(input))
    val machine = VM(compiler.bytecode)
    measure(engine) {
      machine.run()
      machine.lastPoppedStackElem.get
    }
  }

  def evalSlow(): Unit = eval("eval")

  def evalFast(): Unit = eval("eval-fast", fastInput)

  def vmSlow(): Unit = vm("vm")

  def vmFast(): Unit = vm("vm-fast", fastInput)
}
