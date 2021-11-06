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
LIBS           += -lsfml-graphics -lsfml-window -lsfml-system -lcommon

SOURCES += \
    ../other-src/main.cpp \
    ../other-src/spatial-map-unit-tests.cpp \
    ../other-src/spatial-map-unit-tests-n.cpp \
    ../src/detail.cpp \
    ../src/physics.cpp \
    ../src/helpers.cpp

QMAKE_CXXFLAGS += -DMACRO_BUILD_DEMO
SOURCES        += ../other-src/demo.cpp
SOURCES        += ../src/sight.cpp
SOURCES        += ../other-src/sight-unit-tests.cpp
HEADERS        += ../other-src/demo-common.hpp

HEADERS += \
    \#../src/PartitionBoxMap.hpp.bak \
    ../src/SpatialMap.hpp \
    ../src/SpatialMapN.hpp \
    ../src/detail.hpp \
    ../src/helpers.hpp \
    ../inc/aabbtdp/physics.hpp \
    ../other-src/demo.hpp \
    ../inc/aabbtdp/sight.hpp \
    ../inc/aabbtdp/defs.hpp \
    ../src/sight-detail.hpp

INCLUDEPATH += \
    ../lib/cul/inc  \
    ../lib/ecs/inc  \
    ../inc
