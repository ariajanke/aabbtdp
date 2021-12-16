WCXX = /home/aria/documents/emsdk/upstream/emscripten/emcc
CXXFLAGS = -std=c++17 -O3
SOURCES  = $(shell find ../demo-src | grep '[.]cpp$$' | grep -v 'sfmain[.]cpp')
SOURCES += $(shell find ../src | grep '[.]cpp$')
SOURCES += Canvas2dSpa.cpp

OBJECTS_DIR = /media/ramdisk/.release-build-td-physics-demo-wasm-objects
OBJECTS = $(addprefix $(OBJECTS_DIR)/,$(SOURCES:%.cpp=%.o))

$(OBJECTS_DIR)/%.o: | $(OBJECTS_DIR)/src
	$(WCXX) $(CXXFLAGS) -c $*.cpp -o $@
	
$(OBJECTS_DIR)/src:
	mkdir -p $(OBJECTS_DIR)/src
	mkdir -p $(OBJECTS_DIR)/demo-src

EXPORT = 


default: $(OBJECTS)
	@echo $(SOURCES)
	$(WCXX) $(CXXFLAGS) -o bin/spa_out.html $(OBJECTS) -s WASM=1 \
    --shell-file spa_template.html -s NO_EXIT_RUNTIME=1 \
    -DMACRO_AABBTDP_LIBRARY_BUILD_FOR_PERSONAL_ECS_REFERENCE
    
