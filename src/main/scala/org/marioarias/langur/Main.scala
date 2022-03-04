package org.marioarias.langur

import org.marioarias.langur.benchmarks.Benchmarks

@main def benchmark(kind:String): Unit ={
  kind match {
    case "eval" => Benchmarks.evalSlow()
    case "eval-fast" => Benchmarks.evalFast()
    case "vm" => Benchmarks.vmSlow()
    case "vm-fast" => Benchmarks.vmFast()
  }
}

@main def repl():Unit = {
  println("Welcome to langur")
}