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

#include <common/Vector2Util.hpp>

namespace tdp {

namespace detail {

template <typename IterType>
class View {
public:
    View(IterType b_, IterType e_): m_beg(b_), m_end(e_) {}

    IterType begin() const { return m_beg; }

    IterType end() const { return m_end; }

private:
    IterType m_beg, m_end;
};

template <typename ... Types>
using Tuple = std::tuple<Types...>;

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

// ------------------------------ EventRecorder -------------------------------

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

// -------------------------------- FullEntry ---------------------------------

constexpr const int k_default_priority = 0;

struct FullEntry final : public tdp::Entry {
    // record only useful during a frame
    int priority = k_default_priority;
    // (I need some way to handle last appearance)
    bool first_appearance = true;

    // for swptry2
    Vector nudge;

    // board phase boundries
    Real low_x, low_y, high_x, high_y;
};

using EntryEntityRefMap = std::unordered_map<EntityRef, FullEntry, ecs::EntityHasher>;
using EntryMapView      = View<EntryEntityRefMap::iterator>;

void update_broad_boundries(FullEntry &);

void absorb_nudge(FullEntry &);

template <typename Iter, typename ToReference>
void update_broad_boundries(Iter beg, Iter end, ToReference && to_ref) {
    for (auto itr = beg; itr != end; ++itr) {
        FullEntry & ref = to_ref(itr);
        update_broad_boundries(ref);
    }
}

template <typename Iter, typename ToReference>
void absorb_nudges(Iter beg, Iter end, ToReference && to_ref) {
    for (auto itr = beg; itr != end; ++itr) {
        FullEntry & ref = to_ref(itr);
        absorb_nudge(ref);
    }
}

inline void absorb_nudges(EntryMapView entries_view) {
    absorb_nudges(entries_view.begin(), entries_view.end(),
                  [] (auto itr) -> FullEntry & { return itr->second; });
}

inline void update_broad_boundries(
    std::vector<FullEntry *>::iterator beg,
    std::vector<FullEntry *>::iterator end)
{
    update_broad_boundries(beg, end,
        [](std::vector<FullEntry *>::iterator itr) -> FullEntry & { return **itr; });
}

// ----------------------------------------------------------------------------

enum Direction : uint8_t { k_left, k_right, k_down, k_up, k_direction_count };

// does this structure have a purpose?
struct HitSide {
    HitSide() {}
    HitSide(Direction h_, Direction v_):
        horizontal(h_), vertical(v_)
    {}
    Direction horizontal = k_direction_count;
    Direction vertical   = k_direction_count;
};

inline bool are_same(const HitSide & lhs, const HitSide & rhs)
    { return lhs.horizontal == rhs.horizontal && lhs.vertical == rhs.vertical; }

inline bool operator == (const HitSide & lhs, const HitSide & rhs) { return  are_same(lhs, rhs); }

inline bool operator != (const HitSide & lhs, const HitSide & rhs) { return !are_same(lhs, rhs); }

// !I need tests!
/** @returns zero vector if there is no need for push */
std::tuple<Vector, HitSide> find_min_push_displacement
    (const Rectangle &, const Rectangle & other, const Vector & displc);

HitSide trim_displacement_for_barriers
    (const Rectangle &, Vector barriers, Vector & displacement);

HitSide trim_displacement
    (const Rectangle &, const Rectangle & other, Vector & displc);

// much more intense written for growing or shrinking rectangles
bool trespass_occuring
    (const Rectangle &, const Rectangle & other, const Vector & displc);

Rectangle grow(Rectangle, const Size &);

Rectangle grow_by_displacement(Rectangle, const Vector & displc);

Vector find_barrier_for_displacement(
    const Vector & displacement,
    const Vector & positive_barrier, const Vector & negative_barrier);

Rectangle displace(Rectangle, Vector);

#ifdef MACRO_AABBTDP_SHOW_DETAILS_HELPERS

// ----------------------------- level 1 helpers ------------------------------

int large_displacement_step_count
    (const Rectangle &, const Rectangle & other, const Vector & displc);

std::tuple<Vector, HitSide> find_min_push_displacement_small
    (const Rectangle &, const Rectangle & other, const Vector & displc);

HitSide trim_small_displacement
    (const Rectangle &, const Rectangle & other, Vector & displc);

// ----------------------------- level 2 helpers ------------------------------

HitSide values_from_displacement(const Vector &);

#endif

} // end of detail namespace -> into ::tdp

} // end of tdp namespace
