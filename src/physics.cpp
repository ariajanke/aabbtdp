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

#include <aabbtdp/physics.hpp>

#include "physics-interval-sweep.hpp"
#include "physics-quadratic-naive.hpp"
#include "physics-grid.hpp"

namespace {

using namespace cul::exceptions_abbr;
using std::make_unique;

} // end of <anonymous> namespace

namespace tdp {

/* static */ TdpHandlerPtr Physics2DHandler::make_default_instance()
    { return make_unique<tdp::detail::IntervalSweepHandler>(); }

void Physics2DHandler::set_collision_matrix(CollisionMatrix && matrix)
    { set_collision_matrix_(std::move(matrix)); }

void Physics2DHandler::set_collision_matrix(const CollisionMatrix & matrix) {
    auto t = matrix;
    set_collision_matrix_(std::move(t));
}

/* static */ std::unique_ptr<GridPhysicsHandler>
    GridPhysicsHandler::make_instance()
{ return make_unique<tdp::detail::GridPhysicsHandlerImpl>(); }

/* static */ std::unique_ptr<SweepSwitchPhysicsHandler>
    SweepSwitchPhysicsHandler::make_instance()
{ return make_unique<tdp::detail::IntervalSweepHandler>(); }

/* static */ [[noreturn]] std::unique_ptr<AABBTreePhysicsHandler>
    AABBTreePhysicsHandler::make_instance()
{
    throw RtError("AABBTreePhysicsHandler::make_instance: this implementation "
                  "is not finished.");
}

/* static */ TdpHandlerPtr QuadraticPhysicsHandler::make_instance()
    { return make_unique<tdp::detail::Quadratic2DPhysicsImpl>(); }

} // end of tdp namespace

