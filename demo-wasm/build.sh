#!/bin/bash
sources=$(find ../demo-src | grep '[.]cpp$' | grep -v 'sfmain[.]cpp')
sources+=" Canvas2dSpa.cpp "
sources+=$(find ../src | grep '[.]cpp$')
includes="-I../lib/cul/inc -I../lib/ecs/inc -I../lib/HashMap/include -I../inc"
echo $sources
echo $includes
/home/aria/documents/emsdk/upstream/emscripten/emcc -std=c++17 \
    -o bin/spa_out.html $includes $sources -O3 -s WASM=1 \
    --shell-file spa_template.html -s NO_EXIT_RUNTIME=1 \
    -DMACRO_AABBTDP_LIBRARY_BUILD_FOR_PERSONAL_ECS_REFERENCE \
    -s "EXPORTED_RUNTIME_METHODS=['ccall','cwrap','UTF8ToString']"
