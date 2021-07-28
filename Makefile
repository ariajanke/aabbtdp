CXX = g++
LD = g++
CXXFLAGS = -std=c++17 -I./inc -I./lib/cul/inc -I./lib/ecs/inc -O3 -Wall -pedantic -Werror -fno-pretty-templates -DMACRO_PLATFORM_LINUX
SOURCES  = $(shell find src | grep '[.]cpp$$')
OBJECTS_DIR = .release-build
OUTPUT = libaabbtdp.a
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

libaabbtdp.a : default

test: $(OUTPUT)
	$(CXX) $(CXXFLAGS) -L$(shell pwd) -L$(shell pwd)/lib/cul other-src/main.cpp -laabbtdp -lcommon -o .unit-tests
	./.unit-tests
