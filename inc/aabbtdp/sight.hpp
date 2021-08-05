/****************************************************************************

    MIT License

    Copyright (c) 2021 Aria Janke

    Permission is hereby granted, free of charge, to any person obtaining a copy
    of this software and associated documentation files (the "Software"), to deal
    in the Software without restriction, including without limitation the rights
    to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
    copies of the Software, and to permit persons to whom the Software is
    furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all
    copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
    IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
    FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
    AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
    LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
    OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
    SOFTWARE.

*****************************************************************************/

#pragma once

#include <aabbtdp/defs.hpp>

#include <ecs/ecs.hpp>

#include <memory>

namespace tdp {

using ecs::EntityRef;

// ideas describing this are quite complex
class Sighting {
public:
    static std::unique_ptr<Sighting> make_instance();

    // an unobstructed entry with an opacity of 1 will result in a
    // percept with a visibility of 1

    struct Entry {
        EntityRef entity;
        Real      opacity = 1;
        Rectangle bounds;
    };

    // computations concerning whether an entity "notices" a percept, or
    // facing, or anything else is left to the client and is considered out of
    // scope for this library

    struct Percept {
        EntityRef entity;
        Real visibility = 0;
        Vector target;
    };

    virtual ~Sighting() {}

    // going to start with other AABBs first
    // all entries that are added are taken as being in the FOV
    virtual void add_entry(const Entry &) = 0;

    // this can very easily turn into O(n^2), but I'm trying to make an
    // implementation that avoid it (try to reach O(n log n))
    //
    // In game there are many ways I can limit the number of entries sent
    //

    // This function does mutates the state of this Sighting Handler.
    // This call makes this handler ready for more entries to be added for
    // additional calls later
    virtual const std::vector<Percept> & run(Vector source) = 0;
};

} // end of tdp namespace
