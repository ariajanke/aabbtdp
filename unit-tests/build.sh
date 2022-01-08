#!/bin/bash
sources='sight-unit-tests.cpp helpers-unit-tests.cpp main.cpp integration-tests.cpp '
sources+=$(find ../src | grep '[.]cpp$')
includes="-I../lib/cul/inc -I../lib/HashMap/include -I../inc"
libs='-L'$(pwd)'/../lib/cul'
echo $libs
# if I want to use a debugger I'd do it in IDE
# optimizations can often reveal bugs
g++ -std=c++17 -O1 -Wall -pedantic -fno-pretty-templates -DMACRO_PLATFORM_LINUX $libs $sources $includes -o .unit-tests
./.unit-tests
