#!/bin/bash

echo "Running script to create release .zip"

if [ `uname` = "Darwin" ]
then
  mkdir release  
  cp .stack-work/dist/x86_64-osx/Cabal-2.4.0.1/build/notionfy-exe/notionfy-exe ./release/notionfy &&\
  cd release &&\
  zip notionfy_mac.zip notionfy
  tar -zcvf notionfy_mac.tar.gz notionfy
  rm notionfy
  cd ..
else
  mkdir release &&\
  cp .stack-work/dist/x86_64-linux/Cabal-2.4.0.1/build/notionfy-exe/notionfy-exe ./release/notionfy &&\
  cd release &&\
  zip notionfy_x86_64-linux.zip notionfy
  rm notionfy
  cd ..
fi 
