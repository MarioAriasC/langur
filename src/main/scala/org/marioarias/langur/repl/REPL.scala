package org.marioarias.langur.repl

import org.marioarias.langur.compiler.{MCompiler, MCompilerException, SymbolTable}
import org.marioarias.langur.lexer.Lexer
import org.marioarias.langur.objects.{MObject, builtins}
import org.marioarias.langur.parser.Parser
import org.marioarias.langur.vm.{VM, VMException}

import java.io.{InputStream, PrintStream}
import java.util.Scanner
import scala.collection.mutable


/**
 * Created by IntelliJ IDEA.
 *
 * @author Mario Arias
 *         Date: 4/3/22
 *         Time: 12:34 PM
 */
object REPL {
  val MONKEY_FACE =
    """            __,__
     .--.  .-"     "-.  .--.
    / .. \/  .-. .-.  \/ .. \
   | |  '|  /   Y   \  |'  | |
   | \   \  \ 0 | 0 /  /   / |
    \ '- ,\.---------./, -' /
     ''-' /_   ^ ^   _\ '-''
         |  \._   _./  |
         \   \ '~' /   /
          '._ '-=-' _.'
             '-----'
  """

  def start(in: InputStream, out: PrintStream): Unit = {
    val scanner = Scanner(in)
    var constants = mutable.ListBuffer.empty[MObject]
    val globals = mutable.ListBuffer.empty[MObject]
    val symbolTable = SymbolTable()
    builtins.zipWithIndex.foreach { case ((name, _), i) =>
      symbolTable.defineBuiltin(i, name)
    }

    while (scanner.hasNext) {
      out.print(">>> ")
      val code = scanner.nextLine()
      val lexer = Lexer(code)
      val parser = Parser(lexer)
      val program = parser.parseProgram()
      if (parser.errors().nonEmpty) {
        printParsesErrors(out.println, parser.errors())
      } else {
        try {
          val compiler = MCompiler(constants, symbolTable)
          compiler.compile(program)
          val bytecode = compiler.bytecode
          constants = mutable.ListBuffer(bytecode.constants *)
          val machine = VM(bytecode, globals)
          machine.run()
          val stackTop = machine.lastPoppedStackElem
          out.println(stackTop.get.inspect())
        } catch {
          case e: MCompilerException =>
            out.println(s"Woops! Compilation failed:\n ${e.getMessage}")
            out.println(">>>")
          case e: VMException =>
            out.println(s"Woops! Execution bytecode failed:\n ${e.getMessage}")
            out.println(">>>")
        }
      }
    }
  }

  private def printParsesErrors(write: String => Unit, errors: List[String]): Unit = {
    write(MONKEY_FACE)
    write("Woops! we ran into some monkey business here!")
    write(" parser errors:")
    errors.foreach { error =>
      write(s"\t$error")
    }
  }
}
