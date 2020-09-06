#!/bin/bash

# Exit on failure
set -e

mkdir release

if [ $TRAVIS_OS_NAME = windows ]; then
  mv target/graalvm-native-image/notionfys.exe release/notionfy.exe
else
  mv target/native-image/notionfys release/notionfy
  cd release
  zip "notionfy_$TRAVIS_OS_NAME.zip" notionfy
  tar -zcvf "notionfy_$TRAVIS_OS_NAME.tar.gz" notionfy
  rm -f notionfy
fi
