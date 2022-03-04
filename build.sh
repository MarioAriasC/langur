#!/usr/bin/env bash
rm -rf langur-0.1.0-SNAPSHOT
sbt clean universal:packageBin
unzip target/universal/langur-0.1.0-SNAPSHOT.zip