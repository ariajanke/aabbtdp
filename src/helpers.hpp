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
#include <common/Util.hpp>

#include <rigtorp/HashMap.h>

#include <unordered_map>

namespace tdp {

using cul::View;

template <typename ... Types>
using Tuple = std::tuple<Types...>;

// ----------------------------------------------------------------------------

class CollisionEvent final {
public:
    enum Type { k_push, k_rigid, k_trespass };

    CollisionEvent() {}
    CollisionEvent(Entity, Entity, Type);

    Entity first() const { return m_first; }
    Entity second() const { return m_second; }

    Type type() const { return m_type; }

    // !I need tests!
    static bool is_less_than(const CollisionEvent &, const CollisionEvent &);

    bool operator <= (const CollisionEvent & r) const { return compare(r) <= 0; }
    bool operator >= (const CollisionEvent & r) const { return compare(r) >= 0; }
    bool operator <  (const CollisionEvent & r) const { return compare(r) <  0; }
    bool operator >  (const CollisionEvent & r) const { return compare(r) >  0; }
    bool operator != (const CollisionEvent & r) const { return compare(r) != 0; }
    bool operator == (const CollisionEvent & r) const { return compare(r) == 0; }

    void send_to(EventHandler & handler) const;

private:
    static int compare(const CollisionEvent &, const CollisionEvent &);

    int compare(const CollisionEvent &) const;

    Entity m_first, m_second;
    Type m_type;
};

// ------------------------------ EventRecorder -------------------------------

/// EventRecorder's task is not just record events but also prevent old and
/// dupelicate events from reaching the event handler
///
/// If an event is missing for one frame, and is then back. That event will be
/// sent twice, one when it initially occured, and then again when the gap is
/// closed.
///
/// This version contains and sends also trespass events.
class EventRecorder final {
public:
    /// Sends all new events to the handler (old events are filtered out)
    ///
    /// This function rotates out old events for new ones. This is also a side
    /// effect heavy function not only for this object, but does whatever
    /// incurs whatever changes the "on_collision" calls entail.
    void send_events(EventHandler & handler);

    template <typename ... Types>
    void emplace_event(Types ... args) {
        CollisionEvent col_event(std::forward<Types>(args)...);
        push_event(col_event);
    }

private:
    enum EventAge { k_old, k_updated, k_new };
    using CollisionType = CollisionEvent::Type;
    using EventKey      = Tuple<Entity, Entity>;
    using EventValue    = Tuple<CollisionType, EventAge>;

    struct EventHasher final {
        std::size_t operator () (const EventKey &);
    };

    void push_event(const CollisionEvent & col_event);

    using EventContainer = rigtorp::HashMap<EventKey, EventValue, EventHasher>;

    // thank you Erik Rigtorp for saving me the trouble of trying to implement one
    // myself, and for writing such a fast implementation too!
    //
    // There's a problem: I ended up with an iterator with idx == 1 on a
    // container that only had one element :/
    EventContainer m_events = EventContainer{8, std::make_tuple(Entity{}, Entity{})};
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

using EntityHasher =
#ifdef MACRO_AABBTDP_LIBRARY_BUILD_FOR_PERSONAL_ECS_REFERENCE
    ecs::EntityHasher;
#else
    std::hash<Entity>;
#endif

using EntryEntityRefMap = std::unordered_map<Entity, FullEntry, EntityHasher>;
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

HitSide trim_displacement_small
    (const Rectangle &, const Rectangle & other, Vector & displc);

// ----------------------------- level 2 helpers ------------------------------

HitSide values_from_displacement(const Vector &);

#endif

} // end of tdp namespace
