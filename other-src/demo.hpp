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

using Real      = tdp::Real;
using Vector    = tdp::Vector;
using Rectangle = tdp::Rectangle;
using Size2     = tdp::Size;

static constexpr const Real k_inf = std::numeric_limits<Real>::infinity();

struct Velocity : public Vector {
    Velocity() = default;
    Velocity(Real x_, Real y_): Vector(x_, y_) {}

    Vector & operator = (const Vector & r)
        { return (static_cast<Vector &>(*this) = r); }
};

struct MapLimits : public Vector {
    MapLimits(): Vector(k_inf, k_inf) {}
    MapLimits(Real x_, Real y_): Vector(x_, y_) {}
};

struct Name : ecs::InlinedComponent {
    std::string value;
};

struct Pushable {};

struct ColInfo {
    bool hit_wall = false;
    ecs::EntityRef other;
};

template <typename ... Types>
const std::string * get_name_ptr(const ecs::Entity<Types...> & e) {
    if (auto nptr = e.template ptr<Name>()) {
        if (nptr->value.empty()) return nullptr;
        return &nptr->value;
    }
    return nullptr;
}

template <typename ... Types>
const std::string & force_name(const ecs::Entity<Types...> & e) {
    auto a_name = get_name_ptr(e);
    static const std::string k_anon = "<anonymous>";
    return a_name ? *a_name : k_anon;
}

template <typename ... Types>
tdp::Entry to_tdp_entry(ecs::Entity<Types...> entity, tdp::Real elapsed_time, int layer = 0) {
    tdp::Entry entry;
    entry.entity          = entity;
    entry.bounds          = entity.template get<Rectangle>();
    entry.collision_layer = layer;

    if (auto * lims = entity.template ptr<MapLimits>()) {
        entry.barrier = *lims;
    }
    if (auto * velcomp = entity.template ptr<Velocity>()) {
        const auto & vel = static_cast<const Vector &>(* velcomp);
        entry.displacement = vel*elapsed_time;
    }
    if constexpr (cul::TypeList<Types...>::template HasType<Pushable>::k_value)
        entry.pushable = entity.template has<Pushable>();
    return entry;
}

inline tdp::CollisionMatrix make_default_col_matrix() {
    tdp::CollisionMatrix col_matrix;
    col_matrix.set_size(1, 1, tdp::InteractionClass::k_as_solid);
    return col_matrix;
}

#ifdef MACRO_BUILD_DEMO
// using std::enable_if_t won't reduce the number of occurance for this
// preprocessor macro, given there is only one thing to "disable"
void run_demo();
#endif
