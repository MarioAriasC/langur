# Langur

A Scala 3 implementation of the [Monkey Language](https://monkeylang.org/)

Langur has 3 sibling implementations:

* [Kotlin](https://github.com/MarioAriasC/monkey.kt)
* [Go](https://github.com/MarioAriasC/monkey)
* [Crystal](https://github.com/MarioAriasC/monyet)

## Status

The two books ([Writing An Interpreter In Go](https://interpreterbook.com/)
and [Writing A Compiler in Go](https://compilerbook.com/)) are implemented.

## Commands

| Script                           | Description                                                                                                  |
|----------------------------------|--------------------------------------------------------------------------------------------------------------|
| [`tests.sh`](tests.sh)           | Run all the tests                                                                                            |
| [`build.sh`](build.sh)           | Release build                                                                                                |
| [`benchmarks.sh`](benchmarks.sh) | Run the classic monkey benchmark (`fibonacci(35)`), requires one command (`eval`,`eval-fast`,`vm`,`vm-fast`)) |
| [`repl.sh`](repl.sh)             | Run the Langur REPL                                                                                          |

