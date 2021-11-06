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

#include "detail.hpp"

namespace tdp {

/* static */ std::unique_ptr<TopDownPhysicsHandler>
    TopDownPhysicsHandler::make_instance()
{ return std::make_unique<detail::TdpHandlerComplete>(); }

void TopDownPhysicsHandler::set_collision_matrix(CollisionMatrix && matrix)
    { set_collision_matrix_(std::move(matrix)); }

void TopDownPhysicsHandler::set_collision_matrix(const CollisionMatrix & matrix) {
    auto t = matrix;
    set_collision_matrix_(std::move(t));
}


namespace temporary {

TdpHandlerPtr make_quadratic_tdp_physics_instance()
    { return std::make_unique<detail::QuadraticTdpHandler>(); }

} // end of temporary namespace -> into ::tdp

} // end of tdp namespace

