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

// gonna need some things here for both demo and testing

#include <aabbtdp/physics.hpp>
#include <ecs/ecs.hpp>

#include <cassert>

#include <common/Vector2Util.hpp>

using Real      = tdp::Real;
using Vector    = tdp::Vector;
using Rectangle = tdp::Rectangle;
using Size2     = tdp::Size;
using tdp::Physics2DHandler;
using ecs::EntityRef;

using cul::center_of, cul::bottom_of, cul::right_of, cul::convert_to;

template <typename ... Types>
using Tuple = std::tuple<Types...>;

static constexpr const Real k_inf = std::numeric_limits<Real>::infinity();
static constexpr const Real k_field_width  = 600;
static constexpr const Real k_field_height = 600;
