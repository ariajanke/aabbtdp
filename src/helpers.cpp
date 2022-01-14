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
using cul::is_real, cul::magnitude, cul::normalize, std::make_tuple, std::get,
      std::min, std::max;
using tdp::Rectangle, tdp::Vector, tdp::CollisionEvent, tdp::Real;

constexpr const Real k_inf = std::numeric_limits<Real>::infinity();

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
{ return lhs.compare(rhs) < 0; }

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
    for (auto itr = m_events.begin(); itr != m_events.end(); ) {
        auto & age_nfo = itr->second.age;
        if (!age_nfo.has_been_sent) {
            CollisionEvent{get<0>(itr->first), get<1>(itr->first),
                           itr->second.type}
            .send_to(handler);
            age_nfo.has_been_sent = true;
        }
        if (age_nfo.frames_since_last_update != 0) {
            itr = m_events.erase(itr);
            continue;
        } else {
            ++age_nfo.frames_since_last_update;
        }
        ++itr;
    }
}

std::size_t EventRecorder::EventHasher::operator () (const EventKey & key) {
    static constexpr const auto k_half = 8*sizeof(size_t) / 2;
    EntityHasher hash;
    return (  hash(get<1>(key)) << k_half
            | hash(get<1>(key)) >> k_half) ^ hash(get<0>(key));
}

/* private */ void EventRecorder::push_event(const CollisionEvent & col_event) {
    // 1: no events
    // 2: a and b hit; emit event
    // 3: a and b hit; emit nothing

    // 1: no events
    // 2: a and b hit; emit event
    // 3: no events
    // 4: a and b hit; emit event

    using std::make_pair;
    auto key = make_tuple(col_event.first(), col_event.second());
    auto itr = m_events.find(key);
    if (itr == m_events.end()) {
        (void)m_events.insert(make_pair(key, Collision{col_event.type()}));
        return;
    }
    auto & event = itr->second;
    if (event.age.frames_since_last_update > 1) {
        // if it didn't happen last frame, it should be treated as a new event
        event.age.has_been_sent = false;
    }
    event.age.frames_since_last_update = 0;
}

// -------------------------------- FullEntry ---------------------------------

BoardBoundries compute_board_boundries(const FullEntry & entry) {
    return compute_board_boundries(entry.bounds, entry.nudge + entry.displacement,
                                   entry.growth);
}

BoardBoundries compute_board_boundries
    (const Rectangle & bounds, const Vector & full_displacement, const Size & growth)
{
    const Rectangle future_rect = grow(
        Rectangle{top_left_of(bounds) + full_displacement, size_of(bounds)},
        growth);
    return BoardBoundries{
        min(bounds.left, future_rect.left),
        min(bounds.top , future_rect.top ),
        max(right_of (bounds), right_of (future_rect)),
        max(bottom_of(bounds), bottom_of(future_rect))
    };
}

Rectangle as_rectangle(const BoardBoundries & bounds) {
    return Rectangle{
        bounds.low_x, bounds.low_y,
        bounds.high_x - bounds.low_x,
        bounds.high_y - bounds.low_y
    };
}

void absorb_nudge(FullEntry & entry) {
    entry.displacement += entry.nudge;
    entry.nudge         = Vector{};
}

// -------------------------- Helper Implementations --------------------------

enum Side { k_left_side, k_right_side, k_top_side, k_bottom_side, k_side_count };

Real find_highest_non_overlap
    (const Rectangle & subject, const Rectangle & object, const Vector & subject_displc);

Vector find_push_direction
    (const Rectangle & last_non_overlapping_subject, const Rectangle & object,
     const Vector & subject_displacement);

// how long in this (remaining) displacement do we push the object
Real find_push_duration
    (const Rectangle & last_non_overlapping_subject, const Vector & subject_displacement,
     const Rectangle & object, const Vector & object_direction);

template <Direction kt_high_dir, Direction kt_low_dir>
Tuple<Real, Direction> trim_dimension
    (const Real high, const Real low, const Real displc_i, const Real barrier);

/// finds t s.t. displacement*t is most likely to produce a future subject
/// which overlaps the object
Tuple<Real, Side> find_position_of_possible_overlap
    (const Rectangle & subject, const Rectangle & object, const Vector & displc);

Tuple<Vector, HitSide> trim_displacement_small
    (const Rectangle &, const Rectangle & other, const Vector & displc);

Tuple<Vector, HitSide> find_min_push_displacement
    (const Rectangle & rect, const Rectangle & other, const Vector & displc)
{
    // when does the push start? when does it stop?
    // when do they first overlap?
    // what direction should the object be pushed?
    // how far should I push the object?
    const auto start = find_highest_non_overlap(rect, other, displc);
    if (!is_real(start)) return make_tuple(Vector{}, HitSide{});
    const auto lno_subject = displace(rect, displc*start);
    assert(!overlaps(lno_subject, other));
    const auto dir = find_push_direction(lno_subject, other, displc);
    assert(dir.x*displc.x > 0 || dir.y*displc.y > 0);
    // we can only push for a maximum duration of start to end of the frame
    const auto dur = min(
        find_push_duration(lno_subject, displc, other, dir), Real(1) - start);
    static const auto dir_to_side = [] (const Vector & dir) {
        if (dir.x > 0) return HitSide{k_left , k_direction_count};
        if (dir.x < 0) return HitSide{k_right, k_direction_count};
        if (dir.y > 0) return HitSide{k_direction_count, k_up /* top side */};
        assert(dir.y < 0);
        return HitSide{k_direction_count, k_down /* bottom side */};
    };
    if (dir.x != 0) {
        return make_tuple(Vector{displc.x*magnitude(dir.x)*dur, 0}, dir_to_side(dir));
    } else {
        assert(dir.y != 0);
        return make_tuple(Vector{0, displc.y*magnitude(dir.y)*dur}, dir_to_side(dir));
    }
}

Tuple<Vector, HitSide> trim_displacement_for_barriers
    (const Rectangle & rect, Vector barriers, const Vector & displacement)
{
    // parameter assumptions
    assert(is_real(rect.left) && is_real(rect.top));
    assert(is_real(rect.width) && rect.width >= 0);
    assert(is_real(rect.height) && rect.height >= 0);
    assert(!cul::is_nan(barriers.x) && !cul::is_nan(barriers.y));
    assert(is_real(displacement));

    // implementing it in this fashion: it's no longer possible for me to screw
    // up in one dimension, but not the other
    const auto [dis_x, h_dir] = trim_dimension<k_right, k_left>
        (right_of(rect), rect.left, displacement.x, barriers.x);
    const auto [dis_y, v_dir] = trim_dimension<k_down, k_up>
        (bottom_of(rect), rect.top, displacement.y, barriers.y);
    return make_tuple(Vector{dis_x, dis_y}, HitSide{h_dir, v_dir});
}

Tuple<Vector, HitSide> trim_displacement
    (const Rectangle & rect, const Rectangle & other, const Vector & displc)
{
    const auto reduced_displc = get<Real>(find_position_of_possible_overlap(rect, other, displc))*displc;
    return trim_displacement_small(rect, other, reduced_displc);
}

bool trespass_occuring
    (const Rectangle & rect, const Rectangle & other, const Vector & displc)
{
    static auto trespass_occuring_small =
        [](const Rectangle & rect, const Rectangle & other, const Vector & displc)
    { return !overlaps(rect, other) && overlaps(displace(rect, displc), other); };
    const auto reduced_displc = displc*get<Real>(find_position_of_possible_overlap(rect, other, displc));
    return trespass_occuring_small(rect, other, reduced_displc);
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

enum Dimension { k_horizontal, k_vertical };
using PosFunc = Tuple<Real, Side>(*)
    (const Rectangle & subject, const Rectangle & object, const Vector & displc);

using IsLargeDisplacement = bool(*)
    (const Rectangle & subject, const Rectangle & object, const Vector & displc);

template <typename GottenType>
struct TupleLessThan {
    template <typename ... Types>
    bool operator()(const Tuple<Types...> & lhs, const Tuple<Types...> & rhs) const
        { return get<GottenType>(lhs) < get<GottenType>(rhs); }
};

template <Dimension kt_dim>
Real find_push_duration
    (const Rectangle & subject, const Vector & displc, const Rectangle & object);

template <PosFunc low_pf, PosFunc high_pf, IsLargeDisplacement is_large_displacement>
Tuple<Real, Side> position_on_axis
    (const Rectangle & subject, const Rectangle & object, const Vector & displc);

template <Side kt_side>
Tuple<Real, Side> position_on_side
    (const Rectangle & subject, const Rectangle & object, const Vector & displc);

bool is_large_horizontal_displacement
    (const Rectangle & subject, const Rectangle & object, const Vector & displc);

bool is_large_vertical_displacement
    (const Rectangle & subject, const Rectangle & object, const Vector & displc);

HitSide values_from_displacement(const Vector &);

Real find_highest_non_overlap
    (const Rectangle & subject, const Rectangle & object, const Vector & subject_displc)
{
    if (overlaps(subject, object)) { return -k_inf; }
    const auto t = get<Real>(find_position_of_possible_overlap(subject, object, subject_displc));
    if (   std::equal_to<Real>{}(t, 1)
        && !overlaps(displace(subject, subject_displc), object))
    { return k_inf; }
    assert(overlaps(displace(subject, subject_displc*t), object));
    return t*cul::find_highest_false<Real>(
        [subject, object, subject_displc, t](Real u)
        { return overlaps(displace(subject, subject_displc*u*t), object); });
}

Vector find_push_direction
    (const Rectangle & last_non_overlapping_subject,
     const Rectangle & object, const Vector & subject_displacement)
{
    // I want to displace inside the object... just enough to get an
    // intersection rectangle to derive the push direction
    const auto & lno_sub = last_non_overlapping_subject;
    static const auto mag = [](Real x) { return magnitude(x); };
    static const auto make_avail_if_non_ze = [](Real val)
        { return [val](Real x) { return val == 0 ? k_inf : x; }; };
    const auto if_dx_nz = make_avail_if_non_ze(subject_displacement.x);
    const auto if_dy_nz = make_avail_if_non_ze(subject_displacement.y);
    return get<Vector>(min({
        make_tuple(if_dx_nz(mag(lno_sub.left       - right_of (object))), Vector{-1,  0}),
        make_tuple(if_dx_nz(mag(right_of (lno_sub) - object.left      )), Vector{ 1,  0}),
        make_tuple(if_dy_nz(mag(lno_sub.top        - bottom_of(object))), Vector{ 0, -1}),
        make_tuple(if_dy_nz(mag(bottom_of(lno_sub) - object.top       )), Vector{ 0,  1})
    }, TupleLessThan<Real>{}));
}

/// @returns duration in frames where one full displacement is exactly one frame
Real find_push_duration
    (const Rectangle & subject, const Vector & displc,
     const Rectangle & object , const Vector & object_dir)
{
    if (displc.x == 0 || displc.y == 0) return 1;
    assert((object_dir.x == 0) ^ (object_dir.y == 0));
    if (object_dir.x != 0) {
        return find_push_duration<k_vertical>(subject, displc, object);
    } else {
        assert(object_dir.y != 0);
        return find_push_duration<k_horizontal>(subject, displc, object);
    }
}

template <Direction kt_high_dir, Direction kt_low_dir>
Tuple<Real, Direction> trim_dimension
    (const Real high, const Real low, const Real displc_i, const Real barrier)
{
    if (!is_real(barrier)) return make_tuple(displc_i, k_direction_count);

    static const constexpr Real k_bump_fix = 0.00005;
    // what should I use to bump with?
    auto bump = (magnitude(barrier) + (high - low)) / 2;
    /*  */ if (high < barrier && high + displc_i > barrier) {
        const auto displc_out = barrier - high - bump*k_bump_fix;
        assert(high + displc_out < barrier);
        return make_tuple(displc_out, kt_high_dir);
    } else if (low > barrier && low + displc_i < barrier) {
        const auto displc_out = barrier - low + bump*k_bump_fix;
        assert(low + displc_out > barrier);
        return make_tuple(displc_out, kt_low_dir);
    }
    return make_tuple(displc_i, k_direction_count);
}

/// finds t s.t. displacement*t is most likely to produce a future subject
/// which overlaps the object
Tuple<Real, Side> find_position_of_possible_overlap
    (const Rectangle & subject, const Rectangle & object, const Vector & displc)
{
    const auto hmin = position_on_axis
        <position_on_side<k_left_side>, position_on_side<k_right_side>,
         is_large_horizontal_displacement>
        (subject, object, displc);
    const auto vmin = position_on_axis
        <position_on_side<k_top_side>, position_on_side<k_bottom_side>,
         is_large_vertical_displacement>
        (subject, object, displc);
    assert(is_real(get<Real>(hmin)) || is_real(get<Real>(vmin)));
    return min(hmin, vmin, TupleLessThan<Real>{});
}

Tuple<Vector, HitSide> trim_displacement_small
    (const Rectangle & rect, const Rectangle & other, const Vector & displc)
{
    using cul::find_highest_false;
    if (overlaps(rect, other) || !overlaps(displace(rect, displc), other))
        { return make_tuple(displc, HitSide{}); }

    const HitSide hit_parts = values_from_displacement(displc);

    if (!overlaps(displace(rect, Vector(displc.x, 0)), other)) {
        const auto mk_displc = [displc](Real t) { return Vector(displc.x, displc.y*t); };
        const auto ndisplc = mk_displc(find_highest_false<Real>([&](Real t) {
            return overlaps(displace(rect, mk_displc(t)), other);
        }));
        return make_tuple(ndisplc, HitSide(k_direction_count, hit_parts.vertical));
    }

    if (!overlaps(displace(rect, Vector(0, displc.y)), other)) {
        const auto mk_displc = [displc](Real t) { return Vector(displc.x*t, displc.y); };
        const auto ndisplc = mk_displc(find_highest_false<Real>([&](Real t) {
            return overlaps(displace(rect, mk_displc(t)), other);
        }));
        return make_tuple(ndisplc, HitSide(hit_parts.horizontal, k_direction_count));
    }

    // assume the value only changes once
    const auto t = find_highest_false<Real>([&](Real t) {
        return overlaps(displace(rect, t*displc), other);
    });
    // goes both ways
    return make_tuple(displc*t, hit_parts);
}

// ----------------------------- Helpers level 2 ------------------------------

template <Dimension kt_dim>
Real find_portion_overlap
    (const Rectangle & last_non_overlapping_subject, const Rectangle & object, Real direction);

template <Side kt_side>
Real get_side(const Rectangle &);

template <Dimension kt_dim>
Real get_component(const Vector &);

template <Side kt_side>
Real get_component(const Vector &);

template <Dimension kt_dim>
Real find_push_duration
    (const Rectangle & subject, const Vector & displc,
     const Rectangle & object)
{
    const auto comp = get_component<kt_dim>(displc);
    const auto port = find_portion_overlap<kt_dim>(subject, object, comp);
    return port / magnitude(comp);
}

template <PosFunc low_pf, PosFunc high_pf, IsLargeDisplacement is_large_displacement>
Tuple<Real, Side> position_on_axis
    (const Rectangle & subject, const Rectangle & object, const Vector & displc)
{
    if (!is_large_displacement(subject, object, displc))
        return make_tuple(Real(1), k_side_count);
    return min(low_pf(subject, object, displc), high_pf(subject, object, displc),
               TupleLessThan<Real>{});
}

/// Works for large displacements...
/// @returns infinity for no solution (never overlaps)
template <Side kt_side>
Tuple<Real, Side> position_on_side
    (const Rectangle & subject, const Rectangle & object, const Vector & displc)
{
    const auto make_rv = [](Real x) { return make_tuple(x, kt_side); };
    // sort of reverse linear interpolation of x-ways
    const auto dist = (get_side<kt_side>(object) - get_side<kt_side>(subject))
                      / get_component<kt_side>(displc);
    // if greater than 1 or less than 0 -> no solution
    if (dist > 1 || dist < 0) return make_rv(1);
    // test overlap for the resultant rectangle
    // if no overlap -> no solution
    if (!overlaps(displace(subject, displc*dist), object)) return make_rv(1);
    return make_rv(dist);
}

bool is_large_horizontal_displacement
    (const Rectangle & subject, const Rectangle & object, const Vector & displc)
{
    // whether left of right, the offset is essentially canceled out
    const auto future_left = subject.left + displc.x;
    const auto low_x       = min(subject.left, future_left);
    const auto high_x      = max(subject.left, future_left);
    return object.left >= low_x && object.left <= high_x;
}

bool is_large_vertical_displacement
    (const Rectangle & subject, const Rectangle & object, const Vector & displc)
{
    // whether left of right, the offset is essentially canceled out
    const auto future_top = subject.top + displc.y;
    const auto low_y      = min(subject.top, future_top);
    const auto high_y     = max(subject.top, future_top);
    return object.top >= low_y && object.top <= high_y;
}

HitSide values_from_displacement(const Vector & r)
    { return HitSide(r.x > 0 ? k_right : k_left, r.y > 0 ? k_down : k_up); }

// ----------------------------- Helpers level 3 ------------------------------

template <Dimension kt_dim>
Real high_of(const Rectangle &);

template <Dimension kt_dim>
Real low_of(const Rectangle &);

template <Dimension kt_dim>
Real find_portion_overlap
    (const Rectangle & subject, const Rectangle & object, const Real direction)
{
    // assumes rectangles are right next to each other
    // strongly depends on direction
    if (direction > 0) {
        return high_of<kt_dim>(object) - low_of<kt_dim>(subject);
    } else {
        assert(direction < 0);
        return high_of<kt_dim>(subject) - low_of<kt_dim>(object);
    }
}

// C++ is a pain in the a** and can't compare functions at compile time :/
template <Side kt_side>
Real get_side(const Rectangle & rect) {
    switch (kt_side) {
    case k_left_side  : return rect.left;
    case k_right_side : return cul::right_of(rect);
    case k_top_side   : return rect.top;
    case k_bottom_side: return cul::bottom_of(rect);
    }
}

template <Dimension kt_dim>
Real get_component(const Vector & r) {
    switch (kt_dim) {
    case k_horizontal: return r.x;
    case k_vertical  : return r.y;
    }
}

template <Side kt_side>
Real get_component(const Vector & r) {
    switch (kt_side) {
    case k_left_side  : case k_right_side : return r.x;
    case k_top_side   : case k_bottom_side: return r.y;
    }
}

// ----------------------------- Helpers level 4 ------------------------------

template <Dimension kt_dim>
Real low_of(const Rectangle & rect) {
    switch (kt_dim) {
    case k_horizontal: return rect.left;
    case k_vertical  : return rect.top ;
    }
}

template <Dimension kt_dim>
Real high_of(const Rectangle & rect) {
    switch (kt_dim) {
    case k_horizontal: return right_of (rect);
    case k_vertical  : return bottom_of(rect);
    }
}

} // end of tdp namespace
