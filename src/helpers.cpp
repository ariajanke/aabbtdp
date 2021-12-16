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

#include "helpers.hpp"
// miss reference to decrement/increment(ecs::detail::ReferenceCounter*)
#ifdef MACRO_AABBTDP_LIBRARY_BUILD_FOR_PERSONAL_ECS_REFERENCE
#   include <ecs/ecs.hpp>
#endif

#include <cassert>

namespace {

using namespace cul::exceptions_abbr;
using cul::is_real, cul::magnitude, cul::normalize;
using tdp::Rectangle, tdp::Vector, tdp::CollisionEvent;

} // end of <anonymous> namespace

namespace tdp {

// ------------------------------ CollisionEvent ------------------------------

CollisionEvent::CollisionEvent(Entity a, Entity b, Type type):
    m_first(a), m_second(b), m_type(type)
{
    assert(m_first != m_second);
    EntityHasher hash;
    if (hash(m_first) < hash(m_second)) {
        // highest must come first
        std::swap(m_first, m_second);
    }
    if (type != k_rigid && !bool(second())) {
        throw InvArg("CollisionEvent::CollisionEvent: push or rigid types must "
                     "have a both entities be non null.");
    }
}

/* static */ bool CollisionEvent::is_less_than
    (const CollisionEvent & lhs, const CollisionEvent & rhs)
{
    return lhs.compare(rhs) < 0;
}

void CollisionEvent::send_to(EventHandler & handler) const {
    switch (type()) {
    case k_rigid: case k_push:
        handler.on_collision(first(), second(), type() == k_push);
        break;
    case k_trespass:
        handler.on_trespass(first(), second());
        break;
    }
}

/* private static */ int CollisionEvent::compare
    (const CollisionEvent & lhs, const CollisionEvent & rhs)
{ return lhs.compare(rhs); }

/* private */ int CollisionEvent::compare(const CollisionEvent & rhs) const {
    EntityHasher hash;
    if (hash(first ()) < hash(rhs.first ())) return -1;
    if (hash(first ()) > hash(rhs.first ())) return  1;
    if (hash(second()) < hash(rhs.second())) return -1;
    if (hash(second()) > hash(rhs.second())) return  1;
    return int(type()) - int(type());
}


// ------------------------------ EventRecorder -------------------------------

void EventRecorder::send_events(EventHandler & handler) {
    using std::get;
    // there's some reliance on implementation of HashMap knowledge :c
    for (auto itr = m_events.begin(); itr != m_events.end(); ++itr) {
        auto & age = get<EventAge>(itr->second);
        if (age == k_new) {
            CollisionEvent{get<0>(itr->first), get<1>(itr->first),
                           get<CollisionType>(itr->second)}
            .send_to(handler);
        }
        if (age == k_old) {
            m_events.erase(itr);
        } else {
            age = k_old;
        }
    }
}

std::size_t EventRecorder::EventHasher::operator () (const EventKey & key) {
    static constexpr const auto k_half = 8*sizeof(size_t) / 2;
    using std::get;
    EntityHasher hash;
    return (  hash(get<1>(key)) << k_half
            | hash(get<1>(key)) >> k_half) ^ hash(get<0>(key));
}

/* private */ void EventRecorder::push_event(const CollisionEvent & col_event) {
    using std::make_tuple, std::make_pair, std::get;
    auto key = make_tuple(col_event.first(), col_event.second());
    auto itr = m_events.find(key);
    if (itr != m_events.end()) {
        // if it's already present, then it's "updated"
        // if the type has changed, then it's considered a new event
        get<EventAge>(itr->second) = //k_updated;
            get<CollisionType>(itr->second) == col_event.type() ? k_updated : k_new;
    } else {
        (void)m_events.insert(make_pair(key, make_tuple(col_event.type(), k_new)));
    }
}

// -------------------------------- FullEntry ---------------------------------

void update_broad_boundries(FullEntry & entry) {
    static const auto low_x = [](const FullEntry & fe) {
        auto dx = fe.displacement.x + fe.nudge.x;
        return fe.bounds.left + ((dx < 0) ? dx : 0);
    };

    static const auto high_x = [](const FullEntry & fe) {
        auto dx = fe.displacement.x + fe.nudge.x;
        return right_of(fe.bounds) + ((dx > 0) ? dx : 0);
    };

    static const auto low_y = [](const FullEntry & fe) {
        auto dy = fe.displacement.y + fe.nudge.y;
        return fe.bounds.top + ((dy < 0) ? dy : 0);
    };

    static const auto high_y = [](const FullEntry & fe) {
        auto dy = fe.displacement.y + fe.nudge.y;
        return bottom_of(fe.bounds) + ((dy > 0) ? dy : 0);
    };
    entry.low_x  = low_x (entry);
    entry.low_y  = low_y (entry);
    entry.high_x = high_x(entry);
    entry.high_y = high_y(entry);
}

void absorb_nudge(FullEntry & entry) {
    entry.displacement += entry.nudge;
    entry.nudge         = Vector{};
}

// -------------------------- Helper Implementations --------------------------

// 0 means it is not a high displacement
int large_displacement_step_count
    (const Rectangle &, const Rectangle & other, const Vector & displc);

std::tuple<Vector, HitSide> find_min_push_displacement_small
    (const Rectangle &, const Rectangle & other, const Vector & displc);

HitSide trim_displacement_small
    (const Rectangle &, const Rectangle & other, Vector & displc);

template <Direction kt_high_dir, Direction kt_low_dir>
Direction trim_dimension(Real high, Real low, Real & displc_i, Real barrier);

std::tuple<Vector, HitSide> find_min_push_displacement
    (const Rectangle & rect, const Rectangle & other, const Vector & displc)
{
    using std::make_tuple, std::get;
    int steps_for_large = large_displacement_step_count(rect, other, displc);
    if (steps_for_large) {
        // is there a more effecient way to do this?
        // binary search
        // ...perhaps enabling large displacement values
        for (int i = 1; i != steps_for_large; ++i) {
            auto t           = Real(i) / Real(steps_for_large);
            auto displc_part = displc*t;
            auto gv = find_min_push_displacement_small(rect, other, displc_part);
            if (get<Vector>(gv) != Vector()) {
                auto rem = displc*Real(1. - t);
                auto & r = get<Vector>(gv);
                r = r.x != 0 ? Vector(r.x + rem.x, 0) : Vector(0, r.y + rem.y);
                return gv;
            }
        }
        return make_tuple(Vector(), HitSide());
    }
    return find_min_push_displacement_small(rect, other, displc);
}

HitSide trim_displacement_for_barriers
    (const Rectangle & rect, Vector barriers, Vector & displacement)
{
    // parameter assumptions
    assert(is_real(rect.left) && is_real(rect.top));
    assert(is_real(rect.width) && rect.width >= 0);
    assert(is_real(rect.height) && rect.height >= 0);
    assert(!cul::is_nan(barriers.x) && !cul::is_nan(barriers.y));
    assert(is_real(displacement));

    // implementing it in this fashion: it's no longer possible for me to screw
    // up in one dimension, but not the other
    auto h_dir = trim_dimension<k_right, k_left>
        (right_of(rect), rect.left, displacement.x, barriers.x);
    auto v_dir = trim_dimension<k_down, k_up>
        (bottom_of(rect), rect.top, displacement.y, barriers.y);
    return HitSide(h_dir, v_dir);
}

HitSide trim_displacement
    (const Rectangle & rect, const Rectangle & other, Vector & displc)
{
    int steps_for_large = large_displacement_step_count(rect, other, displc);
    if (steps_for_large) {
        for (int i = 1; i != steps_for_large; ++i) {
            auto t           = Real(i) / Real(steps_for_large);
            auto displc_part = displc*t;
            HitSide rv = trim_displacement_small(rect, other, displc_part);
            if (rv != HitSide()) {
                displc = displc_part;
                return rv;
            }
        }
        return HitSide();
    }
    return trim_displacement_small(rect, other, displc);
}

bool trespass_occuring
    (const Rectangle & rect, const Rectangle & other, const Vector & displc)
{
    static auto trespass_occuring_small =
        [](const Rectangle & rect, const Rectangle & other, const Vector & displc)
    {
        return !overlaps(rect, other) && overlaps(displace(rect, displc), other);
    };
    int steps_for_large = large_displacement_step_count(rect, other, displc);
    if (steps_for_large) {
        // there's a "fix" here note: that other locations
        for (int i = 1; i != steps_for_large + 1; ++i) {
            auto t           = Real(i) / Real(steps_for_large);
            auto displc_part = displc*t;
            if (trespass_occuring_small(rect, other, displc_part)) return true;
        }
        return false;
    }
    return trespass_occuring_small(rect, other, displc);
}

Rectangle grow(Rectangle rect, const Size & size_) {
    if (-size_.width > rect.width) {
        rect.left  = rect.left + rect.width*0.5;
        rect.width = 0;
    } else {
        rect.width += size_.width;
        rect.left  -= size_.width*0.5;
    }
    if (-size_.height > rect.height) {
        rect.top    = rect.top + rect.height*0.5;
        rect.height = 0;
    } else {
        rect.height += size_.height;
        rect.top    -= size_.height*0.5;
    }
    return rect;
}

Rectangle grow_by_displacement(Rectangle rect, const Vector & displc) {
    if (displc.x < 0) {
        rect.left  +=  displc.x;
        rect.width += -displc.x;
    } else if (displc.x > 0) {
        rect.width += displc.x;
    }
    if (displc.y < 0) {
        rect.top    +=  displc.y;
        rect.height += -displc.y;
    } else if (displc.y > 0) {
        rect.height += displc.y;
    }
    return rect;
}

Vector find_barrier_for_displacement
    (const Vector & displc, const Vector & pos_bar, const Vector & neg_bar)
{
    return Vector(displc.x > 0 ? pos_bar.x : neg_bar.x,
                  displc.y > 0 ? pos_bar.y : neg_bar.y);
}

Rectangle displace(Rectangle rv, Vector r) {
    rv.left += r.x;
    rv.top  += r.y;
    return rv;
}

// ----------------------------- Helpers level 1 ------------------------------

HitSide values_from_displacement(const Vector &);

int large_displacement_step_count
    (const Rectangle & rect, const Rectangle & other, const Vector & displc)
{
    using std::max, std::ceil, std::make_tuple, std::get;
    auto displc_mag = Vector(magnitude(displc.x), magnitude(displc.y));
    if (   displc_mag.x > other.width  || displc_mag.x > rect.width
        || displc_mag.y > other.height || displc_mag.y > rect.height)
    {
        // this could be further optimized by checking if the rectangle
        // expanded by the displacement overlaps the other rectangle and fast
        // fail out of this function
        //
        // cost: complexity in implementation
        // maybe something I can test later
        return 1 + max(
            ceil( displc_mag.x / max(other.width , rect.width ) ),
            ceil( displc_mag.y / max(other.height, rect.height) ));
    }
    return 0;
}

std::tuple<Vector, HitSide> find_min_push_displacement_small
    (const Rectangle & rect, const Rectangle & other, const Vector & displc)
{
    using std::max, std::min, std::make_tuple;
    static const auto k_no_hit = make_tuple(Vector(), HitSide());
    auto frect = displace(rect, displc);
    if (overlaps(rect, other) || !overlaps(frect, other)) return k_no_hit;

    auto inner_left   = max(frect.left      , other.left      );
    auto inner_right  = min(right_of(frect) , right_of(other) );
    auto inner_top    = max(frect.top       , other.top       );
    auto inner_bottom = min(bottom_of(frect), bottom_of(other));
    if (inner_left >= inner_right || inner_top >= inner_bottom) return k_no_hit;

    Size overlap_area(inner_right - inner_left, inner_bottom - inner_top);
    HitSide hit_parts = values_from_displacement(displc);
    if ((overlap_area.width > overlap_area.height || displc.x == 0) && displc.y != 0) {
        return make_tuple(Vector (0, normalize(displc.y)*overlap_area.height),
                          HitSide(k_direction_count, hit_parts.vertical));
    }
    return make_tuple(Vector (normalize(displc.x)*overlap_area.width, 0),
                      HitSide(hit_parts.horizontal, k_direction_count));
}

HitSide trim_displacement_small
    (const Rectangle & rect, const Rectangle & other, Vector & displc)
{
    using cul::find_highest_false;
    if (overlaps(rect, other) || !overlaps(displace(rect, displc), other))
    { return HitSide(); }

    HitSide hit_parts = values_from_displacement(displc);

    if (!overlaps(displace(rect, Vector(displc.x, 0)), other)) {
        auto mk_displc = [displc](Real t) { return Vector(displc.x, displc.y*t); };
        displc = mk_displc(find_highest_false<Real>([&](Real t) {
            return overlaps(displace(rect, mk_displc(t)), other);
        }));
        return HitSide(k_direction_count, hit_parts.vertical);
    }

    if (!overlaps(displace(rect, Vector(0, displc.y)), other)) {
        auto mk_displc = [displc](Real t) { return Vector(displc.x*t, displc.y); };
        displc = mk_displc(find_highest_false<Real>([&](Real t) {
            return overlaps(displace(rect, mk_displc(t)), other);
        }));
        return HitSide(hit_parts.horizontal, k_direction_count);
    }

    // assume the value only changes once
    auto t = find_highest_false<Real>([&](Real t) {
        return overlaps(displace(rect, t*displc), other);
    });
    // goes both ways
    displc = displc*t;
    return hit_parts;
}


template <Direction kt_high_dir, Direction kt_low_dir>
Direction trim_dimension(Real high, Real low, Real & displc_i, Real barrier) {
    if (!is_real(barrier)) return k_direction_count;

    static const constexpr Real k_bump_fix = 0.00005;
    // what should I use to bump with?
    auto bump = (magnitude(barrier) + (high - low)) / 2;
    /*  */ if (high < barrier && high + displc_i > barrier) {
        displc_i = barrier - high - bump*k_bump_fix;
        assert(high + displc_i < barrier);
        return kt_high_dir;
    } else if (low > barrier && low + displc_i < barrier) {
        displc_i = barrier - low + bump*k_bump_fix;
        assert(low + displc_i > barrier);
        return kt_low_dir;
    }
    return k_direction_count;
}

// ----------------------------- Helpers level 2 ------------------------------

HitSide values_from_displacement(const Vector & r)
    { return HitSide(r.x > 0 ? k_right : k_left, r.y > 0 ? k_down : k_up); }

} // end of tdp namespace
