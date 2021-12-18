CXX = g++
LD = g++
# I need new tooling for make...
CXXFLAGS = -pg -std=c++17 -I./inc -I./lib/cul/inc -I./lib/ecs/inc -I./lib/HashMap/include -O3 -Wall -pedantic -Werror -fno-pretty-templates -DMACRO_PLATFORM_LINUX -DMACRO_AABBTDP_LIBRARY_BUILD_FOR_PERSONAL_ECS_REFERENCE
SOURCES  = $(shell find src | grep '[.]cpp$$')
OBJECTS_DIR = .release-build
OUTPUT = libaabbp.a
OBJECTS = $(addprefix $(OBJECTS_DIR)/,$(SOURCES:%.cpp=%.o))

DEMO_SOURCES = $(shell find demo-src | grep '[.]cpp$$')
DEMO_OBJECTS = $(addprefix $(OBJECTS_DIR)/,$(DEMO_SOURCES:%.cpp=%.o))

UNIT_TEST_SOURCES = $(shell find unit-tests | grep '[.]cpp$$')
UNIT_TEST_OBJECTS = $(addprefix $(OBJECTS_DIR)/,$(UNIT_TEST_SOURCES:%.cpp=%.o))

$(OBJECTS_DIR)/%.o: | $(OBJECTS_DIR)/src
	$(CXX) $(CXXFLAGS) -c $*.cpp -o $@

default: $(OBJECTS) CXXFLAGS+= 
	@echo $(SOURCES)
	ar rvs $(OUTPUT) $(OBJECTS)

$(OBJECTS_DIR)/src:
	mkdir -p $(OBJECTS_DIR)/src
	mkdir -p $(OBJECTS_DIR)/demo-src

clean:
	rm -rf $(OBJECTS_DIR)

libaabbp.a : default

#test: $(OUTPUT)
#	$(CXX) $(CXXFLAGS) -L$(shell pwd) -L$(shell pwd)/lib/cul $(UNIT_TEST_OBJECTS) -laabbp -lcommon -o .unit-tests
#	./.unit-tests

demo: $(OUTPUT) $(DEMO_OBJECTS) CXXFLAGS+= -DMACRO_AABBTDP_LIBRARY_BUILD_FOR_PERSONAL_ECS_REFERENCE
	$(CXX) $(CXXFLAGS) -L$(shell pwd) -L$(shell pwd)/lib/cul $(DEMO_OBJECTS) -lsfml-system -lsfml-window -lsfml-graphics -laabbp -lcommon -o .demo
