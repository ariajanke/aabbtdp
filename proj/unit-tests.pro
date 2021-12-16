QT      -= core gui
TEMPLATE = app
CONFIG  -= c++11
CONFIG  += c++17
TARGET  = aabb-physics-unit-tests

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
    ../unit-tests/main.cpp \
    ../unit-tests/spatial-map-unit-tests.cpp \
    ../unit-tests/spatial-map-unit-tests-n.cpp \
    ../unit-tests/helpers-unit-tests.cpp \
    ../unit-tests/sight-unit-tests.cpp \
    ../unit-tests/integration-tests.cpp \
    \ # Library source
    ../src/physics.cpp \
    ../src/helpers.cpp \
    ../src/physics-interval-sweep.cpp \
    ../src/physics-quadratic-naive.cpp \
    ../src/physics-grid.cpp \
    ../src/physics-aabb-tree.cpp \
    ../src/CollisionHandler.cpp \
    ../src/sight.cpp

HEADERS += \
    \ # Library Interface
    ../inc/aabbtdp/physics.hpp \
    ../inc/aabbtdp/sight.hpp \
    ../inc/aabbtdp/defs.hpp \
    \ # Library Private Headers
    ../src/SpatialMap.hpp \
    ../src/SpatialMapN.hpp \
    ../src/helpers.hpp \
    ../src/physics-interval-sweep.hpp \
    ../src/physics-quadratic-naive.hpp \
    ../src/physics-grid.hpp \
    ../src/physics-aabb-tree.hpp \
    ../src/sight-detail.hpp \
    ../src/CollisionHandler.hpp

INCLUDEPATH += \
    ../lib/cul/inc  \
    ../lib/ecs/inc  \
    ../inc \
    ../lib/HashMap/include
