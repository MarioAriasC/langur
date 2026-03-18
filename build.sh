#!/usr/bin/env zsh
PATH="$PATH:$GRAALVM_HOME/bin"
sbt clean GraalVMNativeImage/packageBin