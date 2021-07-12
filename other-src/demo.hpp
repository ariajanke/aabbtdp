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

#include <cassert>

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

struct MapLimits {
    MapLimits() {}
    MapLimits(Real x_, Real y_, Real w_, Real h_): value(x_, y_, w_, h_) {}

    Rectangle & operator = (const Rectangle & r) { return (value = r); }
    operator const Rectangle & () const { return value; }

    Rectangle value;
};

struct Growth : public Size2 {
    Growth() = default;
    Growth(Real w_, Real h_): Size2(w_, h_) {}

    Size2 & operator = (const Size2 & sz)
        { return (static_cast<Size2 &>(*this) = sz); }
};

struct Name : ecs::InlinedComponent {
    std::string value;
};

struct Pushable {};
struct Bouncy {};

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


#if 0
inline tdp::CollisionMatrix make_default_col_matrix() {
    tdp::CollisionMatrix col_matrix;
    col_matrix.set_size(1, 1, tdp::InteractionClass::k_as_solid);
    return col_matrix;
}
#endif
namespace layers {

constexpr const int k_block       = 0;
constexpr const int k_floor_mat   = 1;
constexpr const int k_passive     = 2;
constexpr const int k_layer_count = 3;

} // end of layers namespace -> into <anonymous>

inline auto make_collision_matrix() {
    using namespace tdp::interaction_classes;
    auto rv = cul::Grid {
        //             block     , floor mat    , passive
        /* block */ {  k_as_solid, k_as_trespass, k_as_passive },
        /* floor */ {  k_reflect , k_as_passive , k_as_passive },
        /* passv */ {  k_reflect , k_reflect    , k_as_passive }
    };
    assert(rv.width() == layers::k_layer_count && rv.height() == layers::k_layer_count);
    return rv;
}

struct Layer {
    int value = tdp::Entry::k_no_layer;
    int & operator = (int i) { return (value = i); }
    operator int () const { return value; }
};

template <typename ... Types>
tdp::Entry to_tdp_entry(ecs::Entity<Types...> entity, tdp::Real elapsed_time) {
    tdp::Entry entry;
    entry.entity = entity;
    entry.bounds = entity.template get<Rectangle>();
    if (auto * lims = entity.template ptr<MapLimits>()) {
        const Rectangle & bounds = *lims;
        entry.negative_barrier.x = bounds.left;
        entry.negative_barrier.y = bounds.top;
        entry.positive_barrier.x = bounds.left + bounds.width ;
        entry.positive_barrier.y = bounds.top  + bounds.height;
    }
    if (auto * velcomp = entity.template ptr<Velocity>()) {
        const auto & vel = static_cast<const Vector &>(* velcomp);
        entry.displacement = vel*elapsed_time;
    }
    using ListOfTypes = cul::TypeList<Types...>;
    if constexpr (ListOfTypes::template HasType<Pushable>::k_value)
        entry.pushable = entity.template has<Pushable>();
    if constexpr (ListOfTypes::template HasType<Growth>::k_value) {
        if (auto * growth = entity.template ptr<Growth>()) {
            entry.growth.width  = growth->width *elapsed_time;
            entry.growth.height = growth->height*elapsed_time;
        }
    }
    if constexpr (ListOfTypes::template HasType<Layer>::k_value) {
        if (entity.template has<Layer>()) {
            entry.collision_layer = entity.template get<Layer>();
        }
    }
    if (entry.collision_layer == tdp::Entry::k_no_layer)
        entry.collision_layer = layers::k_block;
    return entry;
}

#ifdef MACRO_BUILD_DEMO
// using std::enable_if_t won't reduce the number of occurance for this
// preprocessor macro, given there is only one thing to "disable"
void run_demo();
#endif
