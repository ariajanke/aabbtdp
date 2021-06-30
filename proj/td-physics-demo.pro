QT      -= core gui
TEMPLATE = app
CONFIG  -= c++11
CONFIG  += c++17

linux {
    QMAKE_CXXFLAGS += -DMACRO_PLATFORM_LINUX
    #QMAKE_CXXFLAGS += -DMACRO_SWEEP_PRUNE_ALLOCATION_COUT_MSGS
    QMAKE_CXXFLAGS += -DMACRO_BUILD_DEMO
    contains(QT_ARCH, i386) {
        LIBS += -L../../bin/linux/g++-x86
    } else:contains(QT_ARCH, x86_64) {
        LIBS += -L../../bin/linux/g++-x86_64 \
                -L/usr/lib/x86_64-linux-gnu
    }
    LIBS += "-L$$PWD/../lib/cul"
}

debug {
    TARGET  = td-physics-demo
}
release {
    TARGET  = td-physics-demo
}

QMAKE_CXXFLAGS += -std=c++17 -DMACRO_COMPILER_GCC
QMAKE_LFLAGS   += -std=c++17
LIBS           += -lsfml-graphics -lsfml-window -lsfml-system -lcommon

SOURCES += \
    ../other-src/main.cpp \
    ../src/detail.cpp \
    ../src/physics.cpp \
    ../other-src/demo.cpp

HEADERS += \
    ../src/PartitionBoxMap.hpp \
    ../src/detail.hpp \
    ../inc/aabbtdp/physics.hpp \
    ../other-src/demo.hpp

INCLUDEPATH += \
    ../lib/cul/inc  \
    ../lib/ecs/inc  \
    ../inc
