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
        auto & age_nfo = get<AgeInfo>(itr->second);
        if (!age_nfo.has_been_sent) {
            CollisionEvent{get<0>(itr->first), get<1>(itr->first),
                           get<CollisionType>(itr->second)}
            .send_to(handler);
            age_nfo.has_been_sent = true;
        }
        if (age_nfo.frames_since_last_update != 0) {
            m_events.erase(itr);
        } else {
            age_nfo.frames_since_last_update = 1;
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
        // I need a better way of handling event age
        if (get<CollisionType>(itr->second) == col_event.type()) {
            // what if it's new this frame...?
            get<AgeInfo>(itr->second).frames_since_last_update = 0;
        } else {
            get<AgeInfo>(itr->second) = AgeInfo{};
        }
    } else {
        (void)m_events.insert(make_pair(key, make_tuple(col_event.type(), AgeInfo{})));
    }
}

// -------------------------------- FullEntry ---------------------------------

BoardBoundries compute_board_boundries(const FullEntry & entry) {
    return compute_board_boundries(entry.bounds, entry.nudge + entry.displacement,
                                   entry.growth);
}

BoardBoundries compute_board_boundries
    (const Rectangle & bounds, const Vector & full_displacement, const Size & growth)
{
    using cul::top_left_of, cul::size_of, std::min, std::max, cul::right_of,
          cul::bottom_of;
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
#if 1
Rectangle as_rectangle(const BoardBoundries & bounds) {
    return Rectangle{
        bounds.low_x, bounds.low_y,
        bounds.high_x - bounds.low_x,
        bounds.high_y - bounds.low_y
    };
}
#endif
#if 0
void update_broad_boundries(FullEntry & entry) {
    using cul::top_left_of, cul::size_of, std::min, std::max, cul::right_of,
          cul::bottom_of;
    Rectangle future_rect = grow(Rectangle{
        top_left_of(entry.bounds) + entry.nudge + entry.displacement,
        size_of(entry.bounds)}, entry.growth);
    entry.low_x  = min(entry.bounds.left, future_rect.left);
    entry.low_y  = min(entry.bounds.top , future_rect.top );
    entry.high_x = max(right_of (entry.bounds), right_of (future_rect));
    entry.high_y = max(bottom_of(entry.bounds), bottom_of(future_rect));
}
#endif
void absorb_nudge(FullEntry & entry) {
    entry.displacement += entry.nudge;
    entry.nudge         = Vector{};
}

// -------------------------- Helper Implementations --------------------------

Real left_of  (const Rectangle & rect) { return rect.left; }
Real top_of   (const Rectangle & rect) { return rect.top ; }
Real bottom_of(const Rectangle & rect) { return cul::bottom_of(rect); }
Real right_of (const Rectangle & rect) { return cul::right_of(rect) ; }
Real get_x    (const Vector & r) { return r.x; }
Real get_y    (const Vector & r) { return r.y; }

enum Side { k_left_side, k_right_side, k_top_side, k_bottom_side, k_side_count };
enum Dimension { k_horizontal, k_vertical };

// C++ is a pain in the ass and can't compare functions at compile time :/
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

template <Side kt_side>
Real get_component(const Vector & r) {
    switch (kt_side) {
    case k_left_side  : case k_right_side : return r.x;
    case k_top_side   : case k_bottom_side: return r.y;
    }
}

template <typename GottenType>
struct TupleLessThan {
    template <typename ... Types>
    bool operator()(const Tuple<Types...> & lhs, const Tuple<Types...> & rhs) const
        { return std::get<GottenType>(lhs) < std::get<GottenType>(rhs); }
};



/// Works for large displacements...
/// @returns infinity for no solution (never overlaps)
template <Side kt_side>
Tuple<Real, Side> position_on_side
    (const Rectangle & subject, const Rectangle & object, const Vector & displc)
{
    const auto make_rv = [](Real x) { return std::make_tuple(x, kt_side); };
    // sort of reverse linear interpolation of x-ways
    const auto dist = (get_side<kt_side>(object) - get_side<kt_side>(subject))
                      / get_component<kt_side>(displc);
    // if greater than 1 or less than 0 -> no solution
    if (dist > 1 || dist < 0) return make_rv(k_inf);
    // test overlap for the resultant rectangle
    // if no overlap -> no solution
    if (!overlaps(displace(subject, displc*dist), object)) return make_rv(k_inf);
    return make_rv(dist);
}

#if 0
constexpr auto position_on_left   = position_on_side<k_left_side  >;
constexpr auto position_on_right  = position_on_side<k_right_side >;
constexpr auto position_on_bottom = position_on_side<k_bottom_side>;
constexpr auto position_on_top    = position_on_side<k_top_side   >;

Real position_on_left
    (const Rectangle & subject, const Rectangle & object, const Vector & displc)
{ return position_on_side<left_of, get_x>(subject, object, displc); }

Real position_on_right
    (const Rectangle & subject, const Rectangle & object, const Vector & displc)
{ return position_on_side<cul::right_of, get_x>(subject, object, displc); }

Real position_on_bottom
    (const Rectangle & subject, const Rectangle & object, const Vector & displc)
{ return position_on_side<cul::bottom_of, get_y>(subject, object, displc); }

Real position_on_top
    (const Rectangle & subject, const Rectangle & object, const Vector & displc)
{ return position_on_side<top_of, get_y>(subject, object, displc); }
#endif
using PosFunc = Tuple<Real, Side>(*)
    (const Rectangle & subject, const Rectangle & object, const Vector & displc);

using IsLargeDisplacement = bool(*)
    (const Rectangle & subject, const Rectangle & object, const Vector & displc);

template <PosFunc low_pf, PosFunc high_pf, IsLargeDisplacement is_large_displacement>
Tuple<Real, Side> position_on_axis
    (const Rectangle & subject, const Rectangle & object, const Vector & displc)
{
    using std::min, std::make_tuple;
    if (!is_large_displacement(subject, object, displc))
        return make_tuple(Real(1), k_side_count);
    return min(low_pf(subject, object, displc), high_pf(subject, object, displc),
               TupleLessThan<Real>{});
}

bool is_large_horizontal_displacement
    (const Rectangle & subject, const Rectangle & object, const Vector & displc)
{
    using std::min, std::max;
    // whether left of right, the offset is essentially canceled out
    const auto future_left = subject.left + displc.x;
    const auto low_x       = min(subject.left, future_left);
    const auto high_x      = max(subject.left, future_left);
    return object.left >= low_x && object.left <= high_x;
}

bool is_large_vertical_displacement
    (const Rectangle & subject, const Rectangle & object, const Vector & displc)
{
    using std::min, std::max;
    // whether left of right, the offset is essentially canceled out
    const auto future_top = subject.top + displc.y;
    const auto low_y      = min(subject.top, future_top);
    const auto high_y     = max(subject.top, future_top);
    return object.top >= low_y && object.top <= high_y;
}

/// finds t s.t. displacement*t is most likely to produce a future subject
/// which overlaps the object
Tuple<Real, Side> find_position_of_possible_overlap
    (const Rectangle & subject, const Rectangle & object, const Vector & displc)
{
    using std::min, std::get;
    const auto hmin = position_on_axis
        <position_on_side<k_left_side  >, position_on_side<k_right_side  >, is_large_horizontal_displacement>
        (subject, object, displc);
    const auto vmin = position_on_axis
        <position_on_side<k_top_side>,  position_on_side<k_bottom_side>, is_large_vertical_displacement>
        (subject, object, displc);
    assert(is_real(get<Real>(hmin)) || is_real(get<Real>(vmin)));
    return min(hmin, vmin, TupleLessThan<Real>{});
}
#if 0
// 0 means it is not a high displacement
int large_displacement_step_count
    (const Rectangle &, const Rectangle & other, const Vector & displc);
#endif
std::tuple<Vector, HitSide> find_min_push_displacement_small
    (const Rectangle &, const Rectangle & other, const Vector & displc);
#if 0
HitSide trim_displacement_small
    (const Rectangle &, const Rectangle & other, Vector & displc);
#endif
Tuple<Vector, HitSide> trim_displacement_small
    (const Rectangle &, const Rectangle & other, const Vector & displc);

template <Direction kt_high_dir, Direction kt_low_dir>
Direction trim_dimension(Real high, Real low, Real & displc_i, Real barrier);

Real find_highest_non_overlap
    (const Rectangle & subject, const Rectangle & object, const Vector & subject_displc);

Real find_highest_non_overlap
    (const Rectangle & subject, const Rectangle & object, const Vector & subject_displc)
{
    if (overlaps(subject, object)) { return -k_inf; }
    const auto t = std::get<Real>(find_position_of_possible_overlap(subject, object, subject_displc));
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
     const Rectangle & object);

// how long in this (remaining) displacement do we push the object
Real find_push_duration
    (const Rectangle & last_non_overlapping_subject, const Vector & subject_displacement,
     const Rectangle & object, const Vector & object_direction);

template <Dimension kt_dim>
Real find_portion_overlap
    (const Rectangle & last_non_overlapping_subject, const Rectangle & object, Real direction);

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

template <Dimension kt_dim>
Real find_push_duration
    (const Rectangle & subject, const Vector & displc,
     const Rectangle & object)
{
    const auto comp = get_component<kt_dim>(displc);
    const auto port = find_portion_overlap<kt_dim>(subject, object, comp);
    return port / magnitude(comp);
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

Vector find_push_direction
    (const Rectangle & last_non_overlapping_subject,
     const Rectangle & object)
{
    using std::make_tuple, std::get, std::min;
    // I want to displace inside the object... just enough to get an
    // intersection rectangle to derive the push direction
    const auto & lno_sub = last_non_overlapping_subject;
    static const auto mag = [](Real x) { return magnitude(x); };
    return get<Vector>(min({
        make_tuple(mag(lno_sub.left       - right_of (object)), Vector{-1,  0}),
        make_tuple(mag(right_of (lno_sub) - object.left      ), Vector{ 1,  0}),
        make_tuple(mag(lno_sub.top        - bottom_of(object)), Vector{ 0, -1}),
        make_tuple(mag(bottom_of(lno_sub) - object.top       ), Vector{ 0,  1})
    }, TupleLessThan<Real>{}));
}

std::tuple<Vector, HitSide> find_min_push_displacement
    (const Rectangle & rect, const Rectangle & other, const Vector & displc)
{
#   if 0 // old linear method
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
#   else
    // when does the push start? when does it stop?
    // when do they first overlap?
    // what direction should the object be pushed?
    // how far should I push the object?
    using std::make_tuple, std::get;
    const auto start = find_highest_non_overlap(rect, other, displc);
    if (!is_real(start)) return make_tuple(Vector{}, HitSide{});
    const auto lno_subject = displace(rect, displc*start);
    assert(!overlaps(lno_subject, other));
    const auto dir = find_push_direction(lno_subject, other);
    assert(dir.x*displc.x > 0 || dir.y*displc.y > 0);
    // we can only push for a maximum duration of start to end of the frame
    const auto dur = std::min(
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
#   if 0
    const auto dispos = find_position_of_possible_overlap(rect, other, displc);

    if (std::equal_to<Real>{}(dispos, 1)) {
        return find_min_push_displacement_small(rect, other, displc);
    } else {


    }
    return find_min_push_displacement_small(
        rect, other, displc*find_position_of_possible_overlap(rect, other, displc));
#   endif
#   endif
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
#if 0
class TriangleStrips {
public:
    using Triple = Tuple<Vector, Vector, Vector>;
    TriangleStrips(const Triple & a, const Triple & b):
        m_count(2),
        m_impl({ a, b, Triple{}, Triple{} })
    {}

    TriangleStrips(const Triple & a, const Triple & b, const Triple & c, const Triple & d):
        m_count(4),
        m_impl({ a, b, c, d })
    {}

    auto begin() const { return m_impl.cbegin(); }
    auto end  () const { return m_impl.cbegin() + m_count; }

private:
    int m_count;
    std::array<Triple, 4> m_impl;
};

TriangleStrips as_strips(const Rectangle &);

Tuple<Vector, Vector, Vector, Vector>
    get_tl_tr_bl_br(const Rectangle &);

TriangleStrips as_strips(const Rectangle & rect) {
    const static auto tri = [](Vector a, Vector b, Vector c) { return std::make_tuple(a, b, c); };
    const auto [tl, tr, bl, br] = get_tl_tr_bl_br(rect);
    return TriangleStrips(tri(tl, tr, bl), tri(tr, bl, br));
}

Tuple<Vector, Vector, Vector, Vector>
    get_tl_tr_bl_br(const Rectangle & rect)
{
    return std::make_tuple(top_left_of   (rect), top_right_of   (rect),
                           bottom_left_of(rect), bottom_right_of(rect));
}

template <std::size_t kt_pos, std::size_t kt_count>
TriangleStrips::Triple get_triple
    (const std::array<Vector, kt_count> & arr)
{
    static_assert(kt_count > kt_pos + 2, "Must not access an invalid index in array.");
    return std::make_tuple(arr[kt_pos + 0], arr[kt_pos + 1], arr[kt_pos + 2]);
}

std::array<Vector, 6> get_polygonal_strip
    (const Rectangle & rect, const Vector & displc)
{
    // if I do it from corners.. I may find a way to reduce redundancy in my
    // code after I write it
    // at this point... we know both displacement values are not trivial, so
    // it will always be from one corner to another
    //
    // This will be a fun thing to run through an optimizer c:
    const auto [otl, otr, obl, obr] = get_tl_tr_bl_br(rect);
    const auto [ntl, ntr, nbl, nbr] = get_tl_tr_bl_br(displace(rect, displc));
    // complement directions are very similar except the middle two points
    /*  */ if (displc.x > 0 && displc.y > 0) { // +, +
        return { otr, ntr, otl, nbr, obl, nbl };
    } else if (displc.x < 0 && displc.y > 0) { // -, +
        return { otl, ntl, otr, nbl, obr, nbr };
    } else if (displc.x > 0 && displc.y < 0) { // +, -
        return { otl, ntl, obl, ntr, obr, nbr };
    } else if (displc.x < 0 && displc.y < 0) { //  -, -
        // here's a guess...
        return { otr, ntr, obr, ntl, obl, nbl };
    }
    throw InvArg("get_polygonal_strip: Assumption failed: function only "
                 "accepts non-trivial values for both components for "
                 "displacement.");
}

TriangleStrips
    get_strips(const Rectangle & rect, const Vector & displc)
{
    // I don't really want to use "error"
    constexpr const Real k_error = 0.0005;
    const bool tiny_x = displc.x*displc.x < k_error*k_error;
    const bool tiny_y = displc.x*displc.x < k_error*k_error;
    if (tiny_x && tiny_y) {
        return as_strips(rect);
    } else if (tiny_x || tiny_y) {
        return as_strips(as_rectangle(compute_board_boundries(rect, displc, Size{})));
    }
    assert(!tiny_x && !tiny_y);
    const auto poly_strip = get_polygonal_strip(rect, displc);
    return TriangleStrips{
        get_triple<0>(poly_strip), get_triple<1>(poly_strip),
        get_triple<2>(poly_strip), get_triple<3>(poly_strip)
    };
}

template <typename T, std::size_t kt_count_a, std::size_t kt_count_b>
constexpr std::array<T, kt_count_a + kt_count_b> concat_arrays
    (const std::array<T, kt_count_a> & a, const std::array<T, kt_count_b> & b)
{
    std::array<T, kt_count_a + kt_count_b> rv;
    for (std::size_t i = 0; i != kt_count_a; ++i)
        rv[i] = a[i];
    for (std::size_t i = 0; i != kt_count_b; ++i)
        rv[i + kt_count_a] = b[i];
    return rv;
}

template <std::size_t kt_index = 0, typename Head, typename ... Types>
constexpr std::array<Head, (1 + sizeof...(Types) - kt_index)>
    to_array(const Tuple<Head, Types...> & tuple)
{
    auto first_as_array = std::array { std::get<kt_index>(tuple) };
    if constexpr (kt_index == sizeof...(Types)) {
        return first_as_array;
    } else {
        return concat_arrays( first_as_array,
                              to_array<kt_index + 1, Head, Types...>(tuple) );
    }

}

bool is_inside_triangle(const TriangleStrips::Triple & triangle, const Vector & pt) {
    using cul::cross, std::get, std::all_of;
    auto products = {
        cross(get<0>(triangle), pt), cross(get<1>(triangle), pt),
        cross(get<2>(triangle), pt) };
    // reminder from geometry: zero cross product = parallel vectors
    if (all_of(products.begin(), products.end(), [](Real x) { return x < 0; }))
        return true;

    return all_of(products.begin(), products.end(), [](Real x) { return x > 0; });
}

Real find_overlapping_portion
    (const Rectangle & rect, const Rectangle & other, const Vector & displc)
{
    const auto board_bounds = as_rectangle(compute_board_boundries(rect, displc, Size{}));
    // fast way out
    if (!overlaps(board_bounds, other)) return 1;
    // long match
    const auto other_pts = to_array(get_tl_tr_bl_br(other));
    for (const auto & triangle : get_strips(rect, displc)) {
        for (const auto & pt : other_pts) {
            is_inside_triangle(triangle, pt);
        }
    }
    // am I over doing this?
    const auto strips = get_strips(rect, displc);
    // if any one point is in any strip then

    const auto [tl, tr, bl, br] = get_tl_tr_bl_br(other);

    {
    ;

    }
    throw "unimplemented";
}
#endif
#if 0
Tuple<HitSide, Vector> trim_displacement
    (const Rectangle & rect, const Rectangle & other, const Vector & displc)
{}
#endif
HitSide trim_displacement
    (const Rectangle & rect, const Rectangle & other, Vector & displc)
{
#   if 0
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
#   endif
    HitSide rv;
    std::tie(displc, rv) = trim_displacement_small(
        rect, other, std::get<Real>(find_position_of_possible_overlap(rect, other, displc))*displc);
    return rv;
}

bool trespass_occuring
    (const Rectangle & rect, const Rectangle & other, const Vector & displc)
{
    static auto trespass_occuring_small =
        [](const Rectangle & rect, const Rectangle & other, const Vector & displc)
    {
        return !overlaps(rect, other) && overlaps(displace(rect, displc), other);
    };
#   if 0
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
#   endif
    return trespass_occuring_small(rect, other,
                                   displc*std::get<Real>(find_position_of_possible_overlap(rect, other, displc)));
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
#if 0
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
#endif
std::tuple<Vector, HitSide> find_min_push_displacement_small
    (const Rectangle & rect, const Rectangle & other, const Vector & displc)
{
    // this was already "functional programming"
    using std::max, std::min, std::make_tuple;
    static const auto k_no_hit = make_tuple(Vector(), HitSide());
    const auto frect = displace(rect, displc);
    if (overlaps(rect, other) || !overlaps(frect, other)) return k_no_hit;

    const auto inner_left   = max(frect.left      , other.left      );
    const auto inner_right  = min(right_of(frect) , right_of(other) );
    const auto inner_top    = max(frect.top       , other.top       );
    const auto inner_bottom = min(bottom_of(frect), bottom_of(other));
    if (inner_left >= inner_right || inner_top >= inner_bottom) return k_no_hit;

    const Size overlap_area(inner_right - inner_left, inner_bottom - inner_top);
    const HitSide hit_parts = values_from_displacement(displc);
    if ((overlap_area.width > overlap_area.height || displc.x == 0) && displc.y != 0) {
        return make_tuple(Vector (0, normalize(displc.y)*overlap_area.height),
                          HitSide(k_direction_count, hit_parts.vertical));
    }
    return make_tuple(Vector (normalize(displc.x)*overlap_area.width, 0),
                      HitSide(hit_parts.horizontal, k_direction_count));
}
#if 0
HitSide trim_displacement_small
    (const Rectangle & rect, const Rectangle & other, Vector & displc)
{
    using cul::find_highest_false;
    if (overlaps(rect, other) || !overlaps(displace(rect, displc), other))
    { return HitSide(); }

    const HitSide hit_parts = values_from_displacement(displc);

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
#else

Tuple<Vector, HitSide> trim_displacement_small
    (const Rectangle & rect, const Rectangle & other, const Vector & displc)
{
    using cul::find_highest_false, std::make_tuple;
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
#endif

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
