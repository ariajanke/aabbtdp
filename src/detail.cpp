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

#include "detail.hpp"

#include <iostream>

#include <cassert>

namespace {

using namespace cul::exceptions_abbr;
using tdp::Vector;
using tdp::detail::Hit;
using tdp::Rectangle;
using tdp::detail::FullEntry;
using tdp::detail::EntryEntityRefMap;
using tdp::detail::PushPair;

Vector find_barrier_for_displacement(
    const Vector & displacement,
    const Vector & positive_barrier, const Vector & negative_barrier);

Rectangle displace(Rectangle rv, Vector r);

inline FullEntry & iter_to_entryref(EntryEntityRefMap::iterator itr)
    { return itr->second; }

inline FullEntry & iter_to_entryref(std::vector<PushPair>::iterator itr)
    { return *itr->pushee; }

} // end of <anonymous> namespace

namespace tdp {

namespace detail {

// --------------------------- Helpers (level 0) ------------------------------

std::tuple<Vector, Hit> find_min_push_displacement
    (const Rectangle &, const Rectangle & other, const Vector & displc);

std::vector<FullEntry *> prioritized_entries
    (EntryEntityRefMap &, std::vector<FullEntry *> &&);

Hit trim_displacement_for_barriers
    (const Rectangle &, Vector barriers, Vector & displacement);

Hit trim_displacement
    (const Rectangle &, const Rectangle & other, Vector & displc);

bool trespass_occuring
    (const Rectangle &, const Rectangle & other, const Vector & displc);

Rectangle grow(Rectangle, const Size &);

Rectangle grow_by_displacement(Rectangle, const Vector & displc);

// ------------------------------ CollisionEvent ------------------------------

CollisionEvent::CollisionEvent(EntityRef a, EntityRef b, bool is_push):
    m_first(a), m_second(b), m_is_push(is_push)
{
    assert(m_first != m_second);
    if (m_first.hash() < m_second.hash()) {
        // highest must come first
        std::swap(m_first, m_second);
    }
}

/* static */ bool CollisionEvent::is_less_than
    (const CollisionEvent & lhs, const CollisionEvent & rhs)
{
    return lhs.compare(rhs) < 0;
}

/* private static */ int CollisionEvent::compare
    (const CollisionEvent & lhs, const CollisionEvent & rhs)
{
    return lhs.compare(rhs);
}

/* private */ int CollisionEvent::compare(const CollisionEvent & rhs) const {
    if (first ().hash() < rhs.first ().hash()) return -1;
    if (first ().hash() > rhs.first ().hash()) return  1;
    if (second().hash() < rhs.second().hash()) return -1;
    if (second().hash() > rhs.second().hash()) return  1;
    return int(is_pushed()) - int(rhs.is_pushed());
}

// ---------------------------- TdpHandlerComplete ----------------------------

/* private */ void TdpHandlerComplete::update_entry(const Entry & entry) {
    if (m_col_matrix.is_empty()) {
        throw InvArg("TdpHandlerComplete::update_entry: cannot update entries "
                     "with collision matrix not being set.");
    }
    if (entry.collision_layer == Entry::k_no_layer) {
        throw InvArg("TdpHandlerComplete::update_entry: entry must specify "
                     "its collision layer.");
    }
    if (!entry.entity) {
        throw InvArg("TdpHandlerComplete::update_entry: this entry needs to "
                     "have its entity reference set (member named \"entity\").");
    }
    if (   !is_real(entry.bounds.left  ) || !is_real(entry.bounds.top   )
        || !is_real(entry.bounds.width ) || !is_real(entry.bounds.height)
        || !is_real(entry.displacement ) || !is_real(entry.growth.width )
        || !is_real(entry.bounds.height))
    {
        throw InvArg("TdpHandlerComplete::update_entry: bounds, growth, and "
                     "displacement must all be in every field real numbers.");
    }
    if (entry.bounds.width < 0 || entry.bounds.height < 0) {
        throw InvArg("TdpHandlerComplete::update_entry: bounds size must be "
                     "non-negative real numbers.");
    }

    static constexpr const auto k_solid_has_growth_msg =
        "TdpHandlerComplete::update_entry: solid entry has growth, the entry "
        "must be either passive or trespass for every layer. (A behavior that "
        "is not implemented is the rationale for this error)";

    if (entry.growth != Size(0, 0)) {
        for (int x = 0; x != m_col_matrix.width(); ++x) {
            if (m_col_matrix(x, entry.collision_layer) == InteractionClass::k_as_solid) {
                throw InvArg(k_solid_has_growth_msg);
            }
        }
    }
    static_cast<tdp::Entry &>(m_entries[entry.entity]) = entry;
}

/* private */ void TdpHandlerComplete::run(EventHandler & handler) {
    clean_up_containers();
    update_pbm(m_pbinfo, m_entries);
    order_and_handle_pushes();
    do_collision_work(handler);
    issue_events(handler);
    for (auto & pair : m_entries) {
        pair.second.first_appearance = false;
    }
}

/* private */ void TdpHandlerComplete::set_collision_matrix_
    (CollisionMatrix && matrix)
{
    using VecI = CollisionMatrix::Vector;
    using namespace interaction_classes;
    if (matrix.width() != matrix.height()) {
        throw InvArg("TdpHandlerComplete::set_collision_matrix_: "
                     "collision matrix must be a square matrix.");
    }

    static auto make_pos_string = [](VecI r)
        { return std::to_string(r.x) + ", " + std::to_string(r.y); };

    for (VecI r; r != matrix.end_position(); r = matrix.next(r)) {
        if (r.x > r.y) continue;
        if (r.x == r.y) {
            if (matrix(r) != k_reflect) continue;
            throw InvArg("TdpHandlerComplete::set_collision_matrix_: "
                         "diagonal elements may not have a reflect value. ("
                         + make_pos_string(r) + ")");
        }
        if (matrix(r) != matrix(r.y, r.x)) {
            if (matrix(r) == k_reflect) {
                matrix(r) = matrix(r.y, r.x);
            } else if (matrix(r.y, r.x) == k_reflect) {
                matrix(r.y, r.x) = matrix(r);
            } else {
                throw InvArg("TdpHandlerComplete::set_collision_matrix_: "
                             "matrix is not symmetric along diagonal, mismatch "
                             "between: " + make_pos_string(r) + " "
                             + make_pos_string(VecI(r.y, r.x)));
            }
        } else {
            // both are "reflect"
            if (matrix(r) == k_reflect) {
                throw InvArg("TdpHandlerComplete::set_collision_matrix_: "
                             "reflect values may not be on the diagonal ("
                             + make_pos_string(r) + ").");
            }
        }
    }
    m_col_matrix = std::move(matrix);
}

template <typename Iter>
/* private */ std::vector<PushPair> TdpHandlerComplete::get_next_pushables
    (Iter beg, Iter end, std::vector<PushPair> && rv)
{
    using std::get;
    rv.clear();
    for (auto itr = beg; itr != end; ++itr) {
        FullEntry & entry = iter_to_entryref(itr);
        for_each(get_grown_rectangle(entry), [this, &entry, &rv](FullEntry & other_entry) {
            auto col_class = m_col_matrix(entry.collision_layer, other_entry.collision_layer);
            if (col_class != InteractionClass::k_as_solid || !other_entry.pushable) return;
            auto gv = find_min_push_displacement(entry.bounds, other_entry.bounds, entry.displacement);
            if (get<Vector>(gv) == Vector()) return;
            rv.emplace_back(&other_entry, &entry, get<Vector>(gv));
            // updates hit flags
            m_col_events.emplace_back(other_entry.entity, entry.entity, true);
        });
    }
    return std::move(rv);
}

/* private */ void TdpHandlerComplete::clean_up_containers() {
    for (auto itr = m_entries.begin(); itr != m_entries.end();) {
        if (itr->second.entity) {
            ++itr;
        } else {
            itr = m_entries.erase(itr);
        }
    }
    for (auto & pair : m_entries) {
        frame_reset(pair.second);
    }
}

/* private */ void TdpHandlerComplete::order_and_handle_pushes() {
    std::vector<PushPair> pushed_entries = get_next_pushables(
        m_entries.begin(), m_entries.end(), std::move(m_recycled_pushpairs_a));
    std::vector<PushPair> recycled_cont = std::move(m_recycled_pushpairs_b);

    for (int safety = 0; !pushed_entries.empty(); ++safety) {
        for (auto & pair : pushed_entries) {
            assert(pair.pushee->pushable);
            pair.pushee->priority = pair.pusher->priority + 1;
            // I think this should take the maximum of either
            // original displacement or nudge displacement (The original
            // displacement was not considered in computing this nudge
            // displacement)
            pair.pushee->displacement += pair.nudge_displacement;
        }
        static constexpr const int k_maximum_push_pair_iterations = 1024;
        if (safety >= k_maximum_push_pair_iterations) {
            // throw/warn/do something
            break;
        }
        recycled_cont = get_next_pushables(pushed_entries.begin(), pushed_entries.end(), std::move(recycled_cont));
        recycled_cont.swap(pushed_entries);

    }
    m_recycled_pushpairs_a = std::move(pushed_entries);
    m_recycled_pushpairs_b = std::move(recycled_cont );
}

/* private */ void TdpHandlerComplete::issue_events(EventHandler & handler) {
    std::sort(m_col_events.begin(), m_col_events.end(), CollisionEvent::is_less_than);
    assert(std::is_sorted(m_col_events.begin(), m_col_events.end(), CollisionEvent::is_less_than));
    auto old_itr = m_old_col_events.begin();
    auto new_itr = m_col_events.begin();
    auto old_end = m_old_col_events.end();
    auto new_end = m_col_events.end();
    while (old_itr != old_end && new_itr != new_end) {
        if (*old_itr == *new_itr) {
            ++old_itr;
            ++new_itr;
            continue;
        }
        if (*old_itr < *new_itr) {
            ++old_itr;
            continue;
        }
        assert(*old_itr > *new_itr);
        new_itr->send_to(handler);
        ++new_itr;
    }
    for (; new_itr != new_end; ++new_itr) {
        new_itr->send_to(handler);
    }
    m_old_col_events.swap(m_col_events);
    m_col_events.clear();
}

/* private */ void TdpHandlerComplete::do_collision_work(EventHandler & handler) {
    auto ordered_entries = prioritized_entries(m_entries, std::move(m_recycled_order));
    m_recycled_order.clear();
    for (auto * entry_ptr : ordered_entries) {
        auto & entry = *entry_ptr;
        Vector barrier = find_barrier_for_displacement
            (entry.displacement, entry.positive_barrier, entry.negative_barrier);
        if (trim_displacement_for_barriers(entry.bounds, barrier, entry.displacement) != Hit()) {
            m_col_events.emplace_back(entry.entity, EntityRef(), false);
        }
        for_each(get_grown_rectangle(entry), [&entry, this, &handler] (FullEntry & other_entry) {
            using namespace interaction_classes;
            switch (m_col_matrix(entry.collision_layer, other_entry.collision_layer)) {
            case k_as_solid:
                if (trim_displacement(entry.bounds, other_entry.bounds, entry.displacement) == Hit()) return;
                m_col_events.emplace_back(entry.entity, other_entry.entity, false);
                break;
            case k_as_trespass: {
                bool first_appearance_overlap = entry.first_appearance && overlaps(entry.bounds, other_entry.bounds);
                bool regular_trespass         = trespass_occuring(entry.bounds, other_entry.bounds, entry.displacement);
                if (!first_appearance_overlap && !regular_trespass) return;
                handler.on_trespass(entry.entity, other_entry.entity);
                break;
            }
            default: break;
            }
        });
        // fine with other bounds being finalized for trespass events
        cul::set_top_left_of(entry.bounds, cul::top_left_of(entry.bounds) + entry.displacement);
        if (entry.growth != Size()) {
            entry.bounds = grow(entry.bounds, entry.growth);
        }
        handler.finalize_entry(entry.entity, entry.bounds);
    }
    m_recycled_order.swap(ordered_entries);
}

/* private static */ void TdpHandlerComplete::update_pbm
    (PbInfo & info, EntryEntityRefMap & eermap)
{
    info.intermediate_container.clear();
    info.intermediate_container.reserve(eermap.size());
    for (auto & pair : eermap) {
        auto & entry = pair.second;
        info.intermediate_container.emplace_back(get_grown_rectangle(entry),
            &entry);
    }
    info.map.set_elements(info.intermediate_container.begin(), info.intermediate_container.end());
}

/* private static */ Rectangle TdpHandlerComplete::get_grown_rectangle
    (const FullEntry & entry)
{
    static constexpr const Real k_boost_size = 0.5;
    return grow(grow_by_displacement(entry.bounds, entry.displacement),
                Size(1, 1)*k_boost_size);
}

// -------------------------- Helper Implementations --------------------------

// 0 means it is not a high displacement
int large_displacement_step_count
    (const Rectangle &, const Rectangle & other, const Vector & displc);

std::tuple<Vector, Hit> find_min_push_displacement_small
    (const Rectangle &, const Rectangle & other, const Vector & displc);

Hit trim_small_displacement
    (const Rectangle &, const Rectangle & other, Vector & displc);

std::tuple<Vector, Hit> find_min_push_displacement
    (const Rectangle & rect, const Rectangle & other, const Vector & displc)
{
    using std::make_tuple, std::get;
    int steps_for_large = large_displacement_step_count(rect, other, displc);
    if (steps_for_large) {
        for (int i = 1; i != steps_for_large; ++i) {
            auto t           = Real(i) / Real(steps_for_large);
            auto displc_part = displc*t;
            auto gv = find_min_push_displacement_small(rect, other, displc_part);
            if (get<Vector>(gv) != Vector()) {
                auto rem = displc*(1. - t);
                auto & r = get<Vector>(gv);
                r = r.x != 0 ? Vector(r.x + rem.x, 0) : Vector(0, r.y + rem.y);
                return gv;
            }
        }
        return make_tuple(Vector(), Hit());
    }
    return find_min_push_displacement_small(rect, other, displc);
}

std::vector<FullEntry *> prioritized_entries
    (EntryEntityRefMap & map, std::vector<FullEntry *> && rv)
{
    rv.clear();
    rv.reserve(map.size());
    for (auto & pair : map) { rv.emplace_back(&pair.second); }
    static auto order_entries = [](const FullEntry * lhs, const FullEntry * rhs) {
        assert(lhs && rhs);
        return lhs->priority < rhs->priority;
    };
    std::sort(rv.begin(), rv.end(), order_entries);
    std::reverse(rv.begin(), rv.end());
    return std::move(rv);
}

Hit trim_displacement_for_barriers
    (const Rectangle & rect, Vector barriers, Vector & displacement)
{
    static const constexpr Real k_bump_fix = 0.00005;
    Hit rv;
    if (is_real(barriers.y)) {
        if (   bottom_of(rect)                  < barriers.y
            && bottom_of(rect) + displacement.y > barriers.y)
        {
            // heading down
            displacement.y   = barriers.y - bottom_of(rect) - k_bump_fix;
            rv.vertical = k_down;
            assert(bottom_of(rect) + displacement.y < barriers.y);
        } else if (   rect.top                  > barriers.y
                   && rect.top + displacement.y < barriers.y)
        {
            // heading up
            displacement.y   = barriers.y - rect.top + k_bump_fix;
            rv.vertical = k_up;
            assert(rect.top + displacement.y > barriers.y);
        }
    }
    if (is_real(barriers.x)) {
        if (   right_of(rect)                  < barriers.x
            && right_of(rect) + displacement.x > barriers.x)
        {
            // heading right
            displacement.x   = barriers.x - right_of(rect) - k_bump_fix;
            rv.horizontal = k_right;
            assert(right_of(rect) + displacement.x < barriers.x);
        } else if (   rect.left                  > barriers.x
                   && rect.left + displacement.x < barriers.x)
        {
            // heading left
            displacement.x   = barriers.x - rect.left + k_bump_fix;
            rv.horizontal = k_left;
            assert(rect.left + displacement.x > barriers.x);
        }
    }
    return rv;
}

Hit trim_displacement
    (const Rectangle & rect, const Rectangle & other, Vector & displc)
{
    int steps_for_large = large_displacement_step_count(rect, other, displc);
    if (steps_for_large) {
        for (int i = 1; i != steps_for_large; ++i) {
            auto t           = Real(i) / Real(steps_for_large);
            auto displc_part = displc*t;
            Hit rv = trim_small_displacement(rect, other, displc_part);
            if (rv != Hit()) {
                displc = displc_part;
                return rv;
            }
        }
        return Hit();
    }
    return trim_small_displacement(rect, other, displc);
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
        for (int i = 1; i != steps_for_large; ++i) {
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

// ----------------------------- Helpers level 1 ------------------------------

Hit values_from_displacement(const Vector &);

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

std::tuple<Vector, Hit> find_min_push_displacement_small
    (const Rectangle & rect, const Rectangle & other, const Vector & displc)
{
    using std::max, std::min, std::make_tuple;
    static const auto k_no_hit = make_tuple(Vector(), Hit());
    auto frect = displace(rect, displc);
    if (overlaps(rect, other) || !overlaps(frect, other)) return k_no_hit;

    auto inner_left   = max(frect.left      , other.left      );
    auto inner_right  = min(right_of(frect) , right_of(other) );
    auto inner_top    = max(frect.top       , other.top       );
    auto inner_bottom = min(bottom_of(frect), bottom_of(other));
    if (inner_left >= inner_right || inner_top >= inner_bottom) return k_no_hit;

    Size overlap_area(inner_right - inner_left, inner_bottom - inner_top);
    Hit hit_parts = values_from_displacement(displc);
    if ((overlap_area.width > overlap_area.height || displc.x == 0) && displc.y != 0) {
        return make_tuple(Vector(0, normalize(displc.y)*overlap_area.height),
                          Hit   (k_direction_count, hit_parts.vertical));
    }
    return make_tuple(Vector(normalize(displc.x)*overlap_area.width, 0),
                      Hit   (hit_parts.horizontal, k_direction_count));
}

Hit trim_small_displacement
    (const Rectangle & rect, const Rectangle & other, Vector & displc)
{
    using cul::find_highest_false;
    if (overlaps(rect, other) || !overlaps(displace(rect, displc), other))
    { return Hit(); }

    Hit hit_parts = values_from_displacement(displc);

    if (!overlaps(displace(rect, Vector(displc.x, 0)), other)) {
        auto mk_displc = [displc](double t) { return Vector(displc.x, displc.y*t); };
        displc = mk_displc(find_highest_false<double>([&](double t) {
            return overlaps(displace(rect, mk_displc(t)), other);
        }));
        return Hit(k_direction_count, hit_parts.vertical);
    }

    if (!overlaps(displace(rect, Vector(0, displc.y)), other)) {
        auto mk_displc = [displc](double t) { return Vector(displc.x*t, displc.y); };
        displc = mk_displc(find_highest_false<double>([&](double t) {
            return overlaps(displace(rect, mk_displc(t)), other);
        }));
        return Hit(hit_parts.horizontal, k_direction_count);
    }

    // assume the value only changes once
    auto t = find_highest_false<double>([&](double t) {
        return overlaps(displace(rect, t*displc), other);
    });
    // goes both ways
    displc = displc*t;
    return hit_parts;
}

// ----------------------------- Helpers level 2 ------------------------------

Hit values_from_displacement(const Vector & r)
    { return Hit(r.x > 0 ? k_right : k_left, r.y > 0 ? k_down : k_up); }

// there are plans for a more accurate "trespass" detection
// which would involve ideas expressed here
#if 0

std::array<Vector, 4> get_rectangle_points(const Rectangle & rect) {
    auto tl = cul::top_left_of(rect);
    return std::array { tl, tl + Vector(rect.width, 0),
        tl + Vector(0, rect.height), tl + Vector(rect.width, rect.height)
    };
}

// this needs unit tests
Rectangle union_of(const Rectangle &, const Rectangle &);

struct GetTlBrRt {
    Real left, right, bottom, top;
};

GetTlBrRt get_top_left_and_bottom_right(const Rectangle &, const Rectangle &);

GetTlBrRt get_top_left_and_bottom_right(const Rectangle & lhs, const Rectangle & rhs) {
    using std::min, std::max;
    GetTlBrRt rv;
    rv.left   = min(lhs.left      , rhs.left      );
    rv.right  = max(right_of(lhs) , right_of(rhs) );
    rv.top    = min(lhs.top       , rhs.top       );
    rv.bottom = max(bottom_of(lhs), bottom_of(rhs));
    return rv;
}

// this needs unit tests too
// (and test with smaller sizes)
std::array<Vector, 8> convex_hull_of(const Rectangle &, const Rectangle &);

Rectangle union_of(const Rectangle & lhs, const Rectangle & rhs) {
    auto gv = get_top_left_and_bottom_right(lhs, rhs);
    return Rectangle(gv.left, gv.top, gv.right - gv.left, gv.bottom - gv.top);
}

std::array<Vector, 8> convex_hull_of(const Rectangle & lhs, const Rectangle & rhs) {
    using std::min, std::max;
    static auto ex_eq = [](Real a, Real b) { return std::equal_to<Real>()(a, b); };
    auto is_hull_point = [&lhs, &rhs] {
        auto ext = get_top_left_and_bottom_right(lhs, rhs);
        return [=] (Real x, Real y) {
            // these are all floating points!
            // but given the formulas, the values should come out as exactly
            // equal
            return    ex_eq(x, ext.left) || ex_eq(x, ext.right )
                   || ex_eq(y, ext.top ) || ex_eq(y, ext.bottom);
        };
    } ();

    std::array<Vector, 8> rv;
    auto witr = rv.begin();
    auto write_out = [&witr, &rv](Real x, Real y) {
        assert(witr != rv.end());
        *witr++ = Vector(x, y);
    };

    for (const auto & rect : { lhs, rhs }) {
        // if rect width  is zero, then right points should be skipped
        // if rect height is zero, then bottom points should be skipped
        // the point is added if either component is equal one extreme or the
        // other for its respective dimension

        // top left
        if (is_hull_point(rect.left, rect.top))
            write_out(rect.left, rect.top);

        // top right
        if (rect.width != 0 && is_hull_point(right_of(rect), rect.top)) {
            write_out(right_of(rect), rect.top);
        }
        // bottom left
        if (rect.height != 0 && is_hull_point(rect.left, bottom_of(rect))) {
            write_out(rect.left, bottom_of(rect));
        }
        // bottom right
        if (   rect.width != 0 && rect.height != 0
            && is_hull_point(right_of(rect), bottom_of(rect)))
        { write_out(right_of(rect), bottom_of(rect)); }
    }

    std::fill(witr, rv.end(), Vector(k_inf, k_inf));
    return rv;
}

bool trespass_occuring
    (const Rectangle & rect,
     const Rectangle & old_other, const Rectangle & new_other)
{
    using cul::overlaps, std::get, std::remove, std::make_tuple;
    // fast fail
    if (!overlaps(rect, union_of(old_other, new_other))) return false;

    // some fast succeeds
    if (old_other.width != 0 && old_other.height != 0) {
        if (overlaps(rect, old_other)) return true;
    }
    if (new_other.width != 0 && new_other.height != 0) {
        if (overlaps(rect, new_other)) return true;
    }

    // the much more laborous part
    auto gv  = convex_hull_of(old_other, new_other);
    auto itr = gv.begin();
    Vector pivot = *itr++;
    const Vector * last_point = &(*itr++);
    assert(is_real(*last_point) && is_real(pivot));
    // itr not end by size of returned array
    auto rect_pts = get_rectangle_points(rect);
    while (is_real(*itr)) {
        for (const auto & v : rect_pts) {
            if (is_inside_triangle(pivot, *last_point, *itr, v)) return true;
        }
        last_point = &*itr;
        ++itr;
        assert(itr != gv.end());
    }
    return false;
}

#endif

} // end of detail namespace -> into ::tdp

} // end of tdp namespace

namespace {

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

} // end of <anonymous> namespace
