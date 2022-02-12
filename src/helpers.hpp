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
#   if 0
    void send_to(EventHandler & handler) const;
#   endif
private:
    static int compare(const CollisionEvent &, const CollisionEvent &);

    int compare(const CollisionEvent &) const;

    Entity m_first  = Entity{};
    Entity m_second = Entity{};
    Type m_type     = Type  {};
};

struct EntityPair final {
    EntityPair() {}

    EntityPair(const Entity & a_, const Entity & b_):
        first (max_hash(a_, b_)),
        second(min_hash(a_, b_))
    { verify_entity_values("EntityPair"); }

    static Entity max_hash(const Entity & a_, const Entity & b_)
        { return hash(a_) >  hash(b_) ? a_ : b_; }

    static Entity min_hash(const Entity & a_, const Entity & b_)
        { return hash(a_) <= hash(b_) ? a_ : b_; }

    static Entity verify_non_null(const Entity & ref_, const char * caller) {
        using namespace cul::exceptions_abbr;
        if (!is_null(ref_)) return ref_;
        throw InvArg(std::string(caller) + ": both entity references must not "
                     "be null.");
    }

    void verify_entity_values(const char * caller) const {
        using namespace cul::exceptions_abbr;
        if (is_null(first)) {
            throw InvArg(std::string(caller) + ": both entities must not be null.");
        } else if (hash(first) == hash(second)) {
            throw InvArg(std::string(caller) + ": both entities must be unique.");
        }
    }

    static std::size_t hash(const Entity & e) {
#       ifdef MACRO_AABBTDP_LIBRARY_BUILD_FOR_PERSONAL_ECS_REFERENCE
        return e.hash();
#       else
        return std::hash<void *>{}(e);
#       endif
    }

    static bool is_null(const Entity & e) {
#       ifdef MACRO_AABBTDP_LIBRARY_BUILD_FOR_PERSONAL_ECS_REFERENCE
        return e.has_expired();
#       else
        return e == nullptr;
#       endif
    }

    std::size_t hash() const noexcept {
        static constexpr const auto k_shift = sizeof(std::size_t)*8 / 2;
        const auto sh = hash(second);
        return hash(first) ^ (sh >> k_shift | sh << k_shift);
    }

    bool operator == (const EntityPair & rhs) const noexcept { return  are_same(rhs); }

    bool operator != (const EntityPair & rhs) const noexcept { return !are_same(rhs); }

    bool are_same(const EntityPair & rhs) const noexcept
        { return first == rhs.first && second == rhs.second; }

    bool either_expired() const noexcept
        { return is_null(first) || is_null(second); }

    Entity first, second;
};

bool operator & (EventOccurrenceType, EventOccurrenceType) noexcept;

inline bool operator &
    (EventOccurrenceType lhs, EventOccurrenceType rhs) noexcept
{
    using EInt = std::underlying_type_t<EventOccurrenceType>;
    return static_cast<EInt>(lhs) & static_cast<EInt>(rhs);
}

// ------------------------------ EventRecorder -------------------------------

/// EventRecorde records events, and emits them to the handler according to
/// preferences and occurence type (started/continued/ended).
///
/// If an event is missing for one frame, and is then back. That event will be
/// sent twice, one when it initially occured, and then again when the gap is
/// closed.
///
/// This version contains and sends also trespass events.
class EventRecorder final {
public:
    using OccurrenceType = EventOccurrenceType;

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

    void set_event_occurence_preference(EventOccurrenceType otype)
        { m_occurence_preference = otype; }
#   if 0
    bool trespass_is_occuring(const Entity & lhs, const Entity & rhs) const;
#   endif
private:
    struct AgeInfo final {
        AgeInfo() {}
        AgeInfo(bool has_been_sent_, int frames_since_last_update_):
            has_been_sent(has_been_sent_),
            frames_since_last_update(frames_since_last_update_)
        {}
        bool has_been_sent = false;
        int frames_since_last_update = 0;
    };

    using CollisionType = CollisionEvent::Type;    
    struct Collision final {
        Collision() {}
        explicit Collision(CollisionType type_): type(type_) {}
        AgeInfo       age;
        CollisionType type;
    };
#   if 0
    using EventKey = Tuple<Entity, Entity>;
#   endif
    struct EventHasher final {
        std::size_t operator () (const EntityPair & pair) const noexcept
            { return pair.hash(); }
    };
#   if 0
    struct KeyEquality final {
        bool operator () (const EventKey & lhs, const EventKey & rhs) const {
            using std::get;
            return get<0>(lhs) == get<0>(rhs) && get<1>(lhs) == get<1>(rhs);
        }
    };
#   endif
    void push_event(const CollisionEvent & col_event);
public:
    using EventContainer = rigtorp::HashMap<EntityPair, Collision, EventHasher>;
private:
    // thank you Erik Rigtorp for saving me the trouble of trying to implement one
    // myself, and for writing such a fast implementation too!
    EventContainer m_events = EventContainer{8, EntityPair{}};

    OccurrenceType m_occurence_preference = OccurrenceType::on_begin;
};

// -------------------------------- FullEntry ---------------------------------

constexpr const int k_default_priority = 0;

struct BoardBoundries {
    BoardBoundries() {}
    BoardBoundries(Real low_x_, Real low_y_, Real high_x_, Real high_y_):
        low_x(low_x_), low_y(low_y_), high_x(high_x_), high_y(high_y_)
    {}

    // board phase boundries
    Real low_x, low_y, high_x, high_y;
};

struct FullEntry final : public tdp::Entry {
    // record only useful during a frame
    int priority = k_default_priority;
#   if 0
    // (I need some way to handle last appearance)
    // (this maybe out-moded!)
    bool first_appearance = true;
#   endif
    // for swptry2
    Vector nudge;

    BoardBoundries board_bounds;
};

using EntityHasher =
#ifdef MACRO_AABBTDP_LIBRARY_BUILD_FOR_PERSONAL_ECS_REFERENCE
    ecs::EntityHasher;
#else
    std::hash<Entity>;
#endif

using EntryEntityRefMap = std::unordered_map<Entity, FullEntry, EntityHasher>;
using EntryMapView      = View<EntryEntityRefMap::iterator>;

BoardBoundries compute_board_boundries(const FullEntry &);
BoardBoundries compute_board_boundries
    (const Rectangle &, const Vector & full_displacement, const Size & growth);

Rectangle as_rectangle(const BoardBoundries &);

void absorb_nudge(FullEntry &);

template <typename Iter, typename ToReference>
void update_broad_boundries(Iter beg, Iter end, ToReference && to_ref) {
    for (auto itr = beg; itr != end; ++itr) {
        FullEntry & ref = to_ref(itr);
        ref.board_bounds = compute_board_boundries(ref);
        //update_broad_boundries(ref);
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
Tuple<Vector, HitSide> find_min_push_displacement
    (const Rectangle &, const Rectangle & other, const Vector & displc);

Tuple<Vector, HitSide> trim_displacement_for_barriers
    (const Rectangle &, Vector barriers, const Vector & displacement);

Tuple<Vector, HitSide> trim_displacement
    (const Rectangle &, const Rectangle & other, const Vector & displc);

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
