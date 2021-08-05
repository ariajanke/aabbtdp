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

#include <cassert>

namespace {

using namespace cul::exceptions_abbr;

[[nodiscard]] static auto guard(bool & b) {
    struct A {
        A(bool & b_): b(b_) { b = true; }
        ~A() { b = false; }
        bool & b;
    };
    return A{b};
}

} // end of <anonymous> namespace

namespace tdp {

namespace detail {

// ------------------------------ SpEntryFactory ------------------------------

SpEntryFactory::MapBase & SpEntryFactory::
    choose_map_for(SetElIterator beg, SetElIterator end, int depth)
{
    auto choice = choose_map_for_iterators<Element, FullEntryHorzGetters, FullEntryVertGetters>
        (beg, end, depth);
    switch (choice) {
    case k_should_split_horz: return ensure_horz_map(get_division_for<Element>(beg, end, FullEntryHorzGetters{}));
    case k_should_split_vert: return ensure_vert_map(get_division_for<Element>(beg, end, FullEntryVertGetters{}));
    case k_should_use_flat  : return m_flat;
    }
    throw std::runtime_error("bad branch");
#   if 0
    if (depth > k_depth_max || end - beg <= k_few_enough_for_flat)
        return m_flat;

    auto hdiv = get_division_for<Element>(beg, end, FullEntryHorzGetters{});
    auto vdiv = get_division_for<Element>(beg, end, FullEntryVertGetters{});
    auto hcounts = get_counts<Element, FullEntryHorzGetters>(hdiv, beg, end);
    auto vcounts = get_counts<Element, FullEntryVertGetters>(vdiv, beg, end);
    if (hcounts.shared < vcounts.shared) {
        if (too_many_shared(hcounts)) return m_flat;
        return ensure_horz_map(hdiv);
    }
    if (too_many_shared(vcounts)) return m_flat;
    return ensure_vert_map(vdiv);
#   endif
}

/* static */ bool SpEntryFactory::too_many_shared
    (const SpatialMapCounts & counts)
    { return counts.shared > (sum_counts(counts)*3) / 4; }

/* private */ SpEntryFactory::HorzMap & SpEntryFactory::ensure_horz_map
    (Real division)
{
    if (!m_horz) m_horz = std::make_unique<HorzMap>();
    m_horz->set_division(division);
    return *m_horz;
}

/* private */ SpEntryFactory::VertMap & SpEntryFactory::ensure_vert_map
    (Real division)
{
    if (!m_vert) m_vert = std::make_unique<VertMap>();
    m_vert->set_division(division);
    return *m_vert;
}

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


// ------------------------------ EventRecorder -------------------------------

void EventRecorder::send_events(EventHandler & handler) {
    std::sort(m_new_events.begin(), m_new_events.end(), CollisionEvent::is_less_than);
    assert(std::is_sorted(m_new_events.begin(), m_new_events.end(), CollisionEvent::is_less_than));
    auto old_itr = m_old_events.begin();
    auto new_itr = m_new_events.begin();
    auto old_end = m_old_events.end();
    auto new_end = m_new_events.end();
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
    m_old_events.swap(m_new_events);
    m_new_events.clear();
}

// ----------------------------- SpatialMapFront ------------------------------

View<SpatialMapFront::PointerIterator> SpatialMapFront::view_of
    (const Rectangle & rectangle)
{
    if (m_nest_guard) {
        throw RtError("SpatialMapFront::view_of: nested calls not allowed");
    }
    auto g = guard(m_nest_guard);
    occupy_with_view_of(rectangle, m_recycled_cont);

    return View{m_recycled_cont.begin(), m_recycled_cont.end()};
}

void SpatialMapFront::occupy_with_view_of(const Rectangle & rect, PointerContainer & cont) const {
    EntrySpatialRef probe;
    probe.bounds = rect;
    cont = m_spatial_map.collect_candidates(probe, std::move(cont));
}

} // end of detail namespace -> into ::tdp

} // end of tdp namespace
