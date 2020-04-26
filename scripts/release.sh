#!/bin/bash

# Exit on failure
set -e

mkdir release

if [ $TRAVIS_OS_NAME = windows ]; then
  mv target/graalvm-native-image/notionfys.exe release/notionfy.exe
else
  mv target/graalvm-native-image/notionfys "release/notionfy_$TRAVIS_OS_NAME"
fi
