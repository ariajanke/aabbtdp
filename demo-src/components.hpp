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

#include "defs.hpp"
#include <common/ColorString.hpp>

using cul::ColorString;

struct Velocity : public Vector {
    Velocity() = default;
    Velocity(Real x_, Real y_): Vector(x_, y_) {}

    Vector & operator = (const Vector & r)
        { return (static_cast<Vector &>(*this) = r); }
    Vector & as_vector() { return static_cast<Vector &>(*this); }
    const Vector & as_vector() const { return static_cast<const Vector &>(*this); }
};

struct MapLimits final {
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

struct Name {
    Name() {}
    std::string & operator = (const char * cstr) { return (value = cstr); }
    std::string & operator = (const std::string & sstr) { return (value = sstr); }
    bool operator == (const char * cstr) const noexcept { return value == cstr; }
    std::string value;
};

struct Pushable final {};

struct Bouncy final {};

struct HudDrawn final {};
#if 0
struct Color final {
    // black by default
    const char * string = "#000000";
    Color & operator = (const char * cstr) { string = cstr; return *this; }
};
#endif
struct Lifetime final {
    Lifetime() {}
    Lifetime & operator = (Real v) { value = v; return *this; }
    Real value = 0;
};

struct DisplayString final {
    DisplayString() {}
    std::string & operator = (const char * cstr) { return (value = cstr); }
    std::string & operator = (const std::string & sstr) { return (value = sstr); }
    bool operator == (const char * cstr) const noexcept { return value == cstr; }
    std::string value;
};

namespace layers {

constexpr const int k_block       = 0;
constexpr const int k_floor_mat   = 1;
constexpr const int k_passive     = 2;
constexpr const int k_layer_count = 3;

} // end of layers namespace -> into <anonymous>

struct Layer final {
    int value = layers::k_passive;
    int & operator = (int i) { return (value = i); }
    operator int () const { return value; }
};

struct Sight final {
    Vector facing = Vector(0, 1);
    Real spread_angle = k_pi * (1. / 12.);
    Real distance = 200;
};

struct FloatRectangles final {
    static constexpr const Real   k_float_duration = 1.8;
    static constexpr const Real   k_spawn_delay    = 0.5;
    static constexpr const Real   k_cycle_max      = k_float_duration + k_spawn_delay;
    static constexpr const Vector k_float_velocity = Vector{ .70710678118*40, -.70710678118*40 };
    static constexpr const int    k_max_rectangles = int( k_cycle_max / k_spawn_delay ) + 1;
    Real time = k_inf;
};

struct TrackTrespassers final {
    // updating occupancy will have to be done synchronously
    std::vector<EntityRef> entities;
};

using Entity = ecs::Entity<
    Rectangle, ColorString, DisplayString, Layer, Name, Lifetime, Sight,
    Pushable, Bouncy, Growth, MapLimits, Velocity, HudDrawn, FloatRectangles,
    TrackTrespassers>;

// --- helpers? --

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
