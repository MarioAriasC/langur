package org.marioarias.langur.main

import org.marioarias.langur.benchmarks.Benchmarks
import org.marioarias.langur.repl.REPL

@main def benchmark(kind: String): Unit = {
  kind match {
    case "eval"      => Benchmarks.evalSlow()
    case "eval-fast" => Benchmarks.evalFast()
    case "vm"        => Benchmarks.vmSlow()
    case "vm-fast"   => Benchmarks.vmFast()
  }
}

@main def repl(): Unit = {
  println("Hello, this is the langur programming language")
  println("JVM implementation " + System.getProperty("java.version"))
  println("Feel free to type any command")
  REPL.start(System.in, System.out)
}
