#!/bin/bash

set -e

# Make 'external' directory
mkdir external
cd external

# digitaljs_peek_poke_tester 
git clone https://github.com/JakubSzczerbinski/digitaljs_peek_poke_tester.git
cd digitaljs_peek_poke_tester
npm install
cd ..

# firrtl
git clone https://github.com/freechipsproject/firrtl.git -b 1.3-release
cd firrtl
sbt compile
sbt assembly
sbt publishLocal
cd ..

# firrtl-interpreter:
git clone https://github.com/freechipsproject/firrtl-interpreter.git -b 1.3-release
cd firrtl-interpreter
sbt compile
sbt publishLocal
cd ..