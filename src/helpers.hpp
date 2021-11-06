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

#include <aabbtdp/physics.hpp>

#include "SpatialMap.hpp"

#include <common/Vector2Util.hpp>

namespace tdp {

namespace detail {

struct FullEntry;

struct EntrySpatialRef {
    EntrySpatialRef() {}
    EntrySpatialRef(FullEntry * ptr_, Rectangle bounds_): entry(ptr_), bounds(bounds_) {}
    FullEntry * entry = nullptr;
    Rectangle bounds;
};

class FullEntryHorzGetters final : public SpatialMapElementGetters<EntrySpatialRef> {
public:
    Real get_low(const EntrySpatialRef & entry) const final
        { return entry.bounds.left; }

    Real get_high(const EntrySpatialRef & entry) const final
        { return cul::right_of(entry.bounds); }
};

class FullEntryVertGetters final : public SpatialMapElementGetters<EntrySpatialRef> {
public:
    Real get_low(const EntrySpatialRef & entry) const final
        { return entry.bounds.top; }

    Real get_high(const EntrySpatialRef & entry) const final
        { return cul::bottom_of(entry.bounds); }
};

enum FactoryChoice { k_should_split_horz, k_should_split_vert, k_should_use_flat };

// stupidly ugly :c
template <typename Element, typename HorzGetters, typename VertGetters, typename IterType>
FactoryChoice choose_map_for_iterators(IterType beg, IterType end, int depth);

class SpEntryFactory final : public SpatialMapFactory<EntrySpatialRef> {
public:
    using Element = EntrySpatialRef;
    MapBase & choose_map_for(SetElIterator beg, SetElIterator end, int depth);
#   if 0
    static constexpr const int k_depth_max           = 1024;
    static constexpr const int k_few_enough_for_flat =   16;
#   endif
    static bool too_many_shared(const SpatialMapCounts & counts);

private:
    using GettersBase = SpatialMapElementGetters<Element>;
    using HorzMap = PartitionedSpatialMap<Element, FullEntryHorzGetters, SpEntryFactory>;
    using VertMap = PartitionedSpatialMap<Element, FullEntryVertGetters, SpEntryFactory>;

    HorzMap & ensure_horz_map(Real division);

    VertMap & ensure_vert_map(Real division);

    FlatSpatialMap<Element> m_flat;
    std::unique_ptr<HorzMap> m_horz;
    std::unique_ptr<VertMap> m_vert;
};

template <typename ValueType, typename ObjIntf>
std::enable_if_t<kt_is_sp_element_getters<ObjIntf, ValueType>, Real>
    get_division_for(SpIterator<ValueType> beg, SpIterator<ValueType> end, ObjIntf)
{
    // beg to end need not be sorted here
    if (end == beg) return 0;

    Real avg = 0;
    for (auto itr = beg; itr != end; ++itr) {
        avg += (ObjIntf{}.get_high(**itr) + ObjIntf{}.get_low(**itr)) / 2;
    }
    avg /= Real(end - beg);

    auto mcounts = get_counts<ValueType, ObjIntf>(avg, beg, end);

    //pivot_sort_around<ValueType, ObjIntf>(beg, end, avg);

    auto mid = beg + (end - beg) / 2;
    auto mid_low  = ObjIntf{}.get_low (**mid);
    auto mid_high = ObjIntf{}.get_high(**mid);
    auto late_div  = mid_high + (mid_high - mid_low)*0.005;
    auto early_div = mid_low  - (mid_high - mid_low)*0.005;
    auto lcounts = get_counts<ValueType, ObjIntf>(late_div , beg, end);
    auto hcounts = get_counts<ValueType, ObjIntf>(early_div, beg, end);

    // at least five iterations... yuck!
    if (prefer_lhs_counts(mcounts, lcounts) && prefer_lhs_counts(mcounts, hcounts)) {
        return avg;
    }
    return (prefer_lhs_counts(lcounts, hcounts)) ? mid_low : mid_high;
#   if 0

    return prefer_lhs_counts(lcounts, ecounts) ? late_div : early_div;
#   endif
#   if 0
    // let's try a weight based on how far it is from the average point
    // so that farther objects more strongly drag the division toward it
    Real x_avg = 0;
    auto get_x_itr = [](SpIterator<ValueType> itr) {
        auto low = ObjIntf{}.get_low(**itr);
        return low + (ObjIntf{}.get_high(**itr) - low) / 2;
    };
    for (auto itr = beg; itr != end; ++itr) {
        x_avg += get_x_itr(itr);
    }
    x_avg /= (end - beg);
    Real sum_diff = 0, sum_weights = 0;
    for (auto itr = beg; itr != end; ++itr) {
        auto pos  = get_x_itr(itr);
        auto diff = cul::magnitude(x_avg - pos);

        sum_diff    += diff;
        sum_weights += diff*pos;
    }
    return sum_weights / sum_diff;
#   endif
}

using FullEntrySpatialMap = SpatialMap<EntrySpatialRef, SpEntryFactory>;

// ----------------------------------------------------------------------------

class CollisionEvent {
public:
    CollisionEvent() {}
    CollisionEvent(EntityRef, EntityRef, bool is_push);

    EntityRef first() const { return m_first; }
    EntityRef second() const { return m_second; }
    bool is_pushed() const { return m_is_push; }

    // !I need tests!
    static bool is_less_than(const CollisionEvent &, const CollisionEvent &);

    bool operator <= (const CollisionEvent & r) const { return compare(r) <= 0; }
    bool operator >= (const CollisionEvent & r) const { return compare(r) >= 0; }
    bool operator <  (const CollisionEvent & r) const { return compare(r) <  0; }
    bool operator >  (const CollisionEvent & r) const { return compare(r) >  0; }
    bool operator != (const CollisionEvent & r) const { return compare(r) != 0; }
    bool operator == (const CollisionEvent & r) const { return compare(r) == 0; }

    void send_to(EventHandler & handler) const
        { handler.on_collision(m_first, m_second, m_is_push); }

private:
    static int compare(const CollisionEvent &, const CollisionEvent &);
    int compare(const CollisionEvent &) const;
    EntityRef m_first, m_second;
    bool m_is_push = false;
};

/// EventRecorder's task is not just record events but also prevent old
/// dupelicate events from reaching the event handler
class EventRecorder final {
public:
    using CollisionEvents = std::vector<CollisionEvent>;

    /// Sends all new events to the handler (old events are filtered out)
    ///
    /// This function rotates out old events for new ones. This is also a side
    /// effect heavy function not only for this object, but does whatever
    /// incurs whatever changes the "on_collision" calls entail.
    void send_events(EventHandler & handler);

    template <typename ... Types>
    void emplace_event(Types ... args)
        { m_new_events.emplace_back(std::forward<Types>(args)...); }

private:
    CollisionEvents m_old_events;
    CollisionEvents m_new_events;
};

// ----------------------------------------------------------------------------

template <typename IterType>
class View {
public:
    View(IterType b_, IterType e_): m_beg(b_), m_end(e_) {}

    IterType begin() const { return m_beg; }

    IterType end() const { return m_end; }

private:
    IterType m_beg, m_end;
};

class SpatialMapFront final {
public:
    using Element          = EntrySpatialRef;
    using ElementContainer = std::vector<EntrySpatialRef>;
    using PointerContainer = SpElementContainer<Element>;
    using PointerIterator  = SpIterator<Element>;

    // full entrys to spatial entry conversions
    template <typename IterType, typename Func>
    void set_elements(IterType beg, IterType end, Func && convert_iterator_to_element);

    // no nested calls allowed
    View<PointerIterator> view_of(const Rectangle &);

    // nest your calls as you please
    void occupy_with_view_of(const Rectangle &, PointerContainer &) const;

private:
    bool m_nest_guard = false;
    FullEntrySpatialMap m_spatial_map;
    PointerContainer m_recycled_cont;
    ElementContainer m_entry_cont;
};

// ----------------------------------------------------------------------------

template <typename Element, typename HorzGetters, typename VertGetters, typename IterType>
FactoryChoice choose_map_for_iterators(IterType beg, IterType end, int depth) {
    static constexpr const int k_depth_max           = 1024;
    static constexpr const int k_few_enough_for_flat =    8;

    //return k_should_use_flat;

    static auto enough_for_fork = [](const SpatialMapCounts & counts) {
        return std::min(counts.on_low, counts.on_high) > (sum_counts(counts) / 12);
    };

    if (depth > k_depth_max || end - beg <= k_few_enough_for_flat)
        return k_should_use_flat;

    auto hdiv = get_division_for<Element>(beg, end, HorzGetters{});
    auto vdiv = get_division_for<Element>(beg, end, VertGetters{});
    auto hcounts = get_counts<Element, HorzGetters>(hdiv, beg, end);
    auto vcounts = get_counts<Element, VertGetters>(vdiv, beg, end);

    if (prefer_lhs_counts(hcounts, vcounts)) {
        if (SpEntryFactory::too_many_shared(hcounts) || !enough_for_fork(hcounts))
            return k_should_use_flat;
        return k_should_split_horz;
    }
    if (SpEntryFactory::too_many_shared(vcounts) || !enough_for_fork(vcounts))
        return k_should_use_flat;
    return k_should_split_vert;
}

// ----------------------------------------------------------------------------

template <typename IterType, typename Func>
void SpatialMapFront::set_elements
    (IterType beg, IterType end, Func && convert_iterator_to_element)
{
    m_entry_cont.clear();
    if constexpr (kt_is_random_access_iterator<IterType>) {
        m_entry_cont.reserve(end - beg);
    }
    for (auto itr = beg; itr != end; ++itr) {
        auto cpy_itr = itr;
        Element el = convert_iterator_to_element(cpy_itr);
        m_entry_cont.push_back(el);
    }
    m_spatial_map.set_elements(m_entry_cont.begin(), m_entry_cont.end());
}

} // end of detail namespace -> into ::tdp

} // end of tdp namespace
