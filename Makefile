CXX = g++
LD = g++
CXXFLAGS = -pg -std=c++17 -I./inc -I./lib/cul/inc -I./lib/ecs/inc -O3 -Wall -pedantic -Werror -fno-pretty-templates -DMACRO_PLATFORM_LINUX
SOURCES  = $(shell find src | grep '[.]cpp$$')
OBJECTS_DIR = .release-build
OUTPUT = libaabbp.a
OBJECTS = $(addprefix $(OBJECTS_DIR)/,$(SOURCES:%.cpp=%.o))

$(OBJECTS_DIR)/%.o: | $(OBJECTS_DIR)/src
	$(CXX) $(CXXFLAGS) -c $*.cpp -o $@

default: $(OBJECTS)
	@echo $(SOURCES)
	ar rvs $(OUTPUT) $(OBJECTS)

$(OBJECTS_DIR)/src:
	mkdir -p $(OBJECTS_DIR)/src

clean:
	rm -rf $(OBJECTS_DIR)

libaabbp.a : default

test: $(OUTPUT)
	$(CXX) $(CXXFLAGS) -L$(shell pwd) -L$(shell pwd)/lib/cul other-src/main.cpp other-src/spatial-map-unit-tests.cpp other-src/sight-unit-tests.cpp -laabbp -lcommon -o .unit-tests
	./.unit-tests

demo: $(OUTPUT)
	$(CXX) $(CXXFLAGS) -L$(shell pwd) -L$(shell pwd)/lib/cul -DMACRO_BUILD_DEMO other-src/demo.cpp other-src/main.cpp other-src/spatial-map-unit-tests.cpp other-src/sight-unit-tests.cpp -lsfml-system -lsfml-window -lsfml-graphics -laabbp -lcommon -o .demo
