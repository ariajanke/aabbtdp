QT      -= core gui
TEMPLATE = app
CONFIG  -= c++11
CONFIG  += c++17
TARGET  = td-physics-demo

linux {
    QMAKE_CXXFLAGS += -DMACRO_PLATFORM_LINUX
    #QMAKE_CXXFLAGS += -DMACRO_SWEEP_PRUNE_ALLOCATION_COUT_MSGS
    contains(QT_ARCH, i386) {
        LIBS += -L../../bin/linux/g++-x86
    } else:contains(QT_ARCH, x86_64) {
        LIBS += -L../../bin/linux/g++-x86_64 \
                -L/usr/lib/x86_64-linux-gnu
    }
    LIBS += "-L$$PWD/../lib/cul"
}

QMAKE_CXXFLAGS += -std=c++17 -DMACRO_COMPILER_GCC
QMAKE_LFLAGS   += -std=c++17
LIBS           += -lsfml-graphics -lsfml-window -lsfml-system -lcommon \
                  -pthread

SOURCES += \
    \# ../demo-src/main.cpp \
    ../src/physics.cpp \
    ../src/helpers.cpp \
    ../src/physics-interval-sweep.cpp \
    ../src/physics-quadratic-naive.cpp \
    ../src/physics-grid.cpp \
    ../src/physics-aabb-tree.cpp \
    ../src/CollisionHandler.cpp \
    ../demo-src/sfmain.cpp \
    ../demo-src/DemoDriver.cpp \
    ../demo-src/systems.cpp \
    ../demo-src/drawing.cpp

QMAKE_CXXFLAGS += -DMACRO_BUILD_DEMO -DMACRO_AABBTDP_LIBRARY_BUILD_FOR_PERSONAL_ECS_REFERENCE
# SOURCES        += ../demo-src/demo.cpp
SOURCES        += ../src/sight.cpp
# HEADERS        += ../demo-src/demo-common.hpp

HEADERS += \
    \#../src/PartitionBoxMap.hpp.bak \
    ../src/SpatialMap.hpp \
    ../src/SpatialMapN.hpp \
    ../src/helpers.hpp \
    ../src/physics-interval-sweep.hpp \
    ../src/physics-quadratic-naive.hpp \
    ../src/physics-grid.hpp \
    ../src/physics-aabb-tree.hpp \
    ../inc/aabbtdp/physics.hpp \
    \# ../demo-src/demo.hpp \
    ../inc/aabbtdp/sight.hpp \
    ../inc/aabbtdp/defs.hpp \
    ../src/sight-detail.hpp \
    ../src/CollisionHandler.hpp \
    ../demo-src/defs.hpp \
    ../demo-src/components.hpp \
    ../demo-src/systems.hpp \
    ../demo-src/DemoDriver.hpp \
    ../demo-src/drawing.hpp \
    ../demo-src/ColorString.hpp

INCLUDEPATH += \
    ../lib/cul/inc  \
    ../lib/ecs/inc  \
    ../inc \
    ../lib/HashMap/include
