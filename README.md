# Langur

A GraalVM Scala 3 implementation of the [Monkey Language](https://monkeylang.org/)

The Scala JVM version is on the [main branch](https://github.com/MarioAriasC/langur)

Langur has 3 sibling implementations:

* [Kotlin](https://github.com/MarioAriasC/monkey.kt)
* [Go](https://github.com/MarioAriasC/monkey)
* [Crystal](https://github.com/MarioAriasC/monyet)

## Status

The two books ([Writing An Interpreter In Go](https://interpreterbook.com/)
and [Writing A Compiler in Go](https://compilerbook.com/)) are implemented.

## Commands

| Script                           | Description                                                                                                   |
|----------------------------------|---------------------------------------------------------------------------------------------------------------|
| [`tests.sh`](tests.sh)           | Run all the tests                                                                                             |
| [`build.sh`](build.sh)           | Release build                                                                                                 |
| [`benchmarks.sh`](benchmarks.sh) | Run the classic monkey benchmark (`fibonacci(35)`), requires one command (`eval`,`eval-fast`,`vm`,`vm-fast`)) |


## REPL

The REPL for Langur GraalVM is currently  not working, we're working to fix it