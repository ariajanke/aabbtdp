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

#include <common/Util.hpp>
#include <common/Vector2Util.hpp>

#include "PartitionBoxMap.hpp"

// Everything in this file is meant to contain implementation details that a
// client coder need not see
//
// however for the purposes of testing it's important to at some level reveal
// the code

namespace tdp {

namespace detail {

constexpr const int k_default_priority = -1;

using namespace cul::exceptions_abbr;
using cul::is_real, cul::bottom_of, cul::right_of, cul::normalize,
      cul::magnitude;

enum Direction : uint8_t { k_left, k_right, k_down, k_up, k_direction_count };

// ----------------------------------------------------------------------------

struct FullEntry : tdp::Entry {
    // record only useful during a frame
    int priority = k_default_priority;
    // (I need some way to handle last appearance)
    bool first_appearance = true;
};

inline void frame_reset(FullEntry & entry) {
    entry.priority = k_default_priority;
}

// ----------------------------------------------------------------------------

struct PushPair {
    PushPair() {}
    PushPair(FullEntry * pushee_, FullEntry * pusher_, Vector nud_displc):
        pushee(pushee_), pusher(pusher_), nudge_displacement(nud_displc) {}

    FullEntry * pushee = nullptr;
    FullEntry * pusher = nullptr;

    Vector nudge_displacement;
};

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

using EntryEntityRefMap = std::unordered_map<EntityRef, FullEntry, ecs::EntityHasher>;

// ----------------------------------------------------------------------------

class TdpHandlerComplete final : public TopDownPhysicsHandler {
    void update_entry(const Entry & entry) final;

    void run(EventHandler &) final;

    void set_collision_matrix_(CollisionMatrix && matrix) final;

    template <typename Func>
    void for_each(const Rectangle & bounds, Func && f) {
        m_pbinfo.map.for_each(bounds, [&f](FullEntry * feptr) { f(*feptr); });
    }

    template <typename Iter>
    [[nodiscard]] std::vector<PushPair> get_next_pushables
        (Iter beg, Iter end, std::vector<PushPair> && rv = std::vector<PushPair>());

    void clean_up_containers();

    void order_and_handle_pushes();

    void issue_events(EventHandler &);

    void do_collision_work(EventHandler &);

    const CollisionMatrix & collision_matrix() const final
        { return m_col_matrix; }

    struct PbInfo {
        PartitionBoxMap<FullEntry *, k_pbm_use_flat_only> map;
        PbmContainer<FullEntry *> intermediate_container;
    };
    PbInfo m_pbinfo;

    // does not modify entity -> entry map, but does take writable references
    static void update_pbm(PbInfo &, EntryEntityRefMap &);

    static Rectangle get_grown_rectangle(const FullEntry &);

    // ofc this means, each new entry will be asking for a few thousands of
    // instructions to allocate a map entry
    EntryEntityRefMap m_entries;
    std::vector<CollisionEvent> m_col_events;
    std::vector<CollisionEvent> m_old_col_events;

    CollisionMatrix m_col_matrix;

    // recycled containers
    std::vector<PushPair> m_recycled_pushpairs_a, m_recycled_pushpairs_b;
    std::vector<FullEntry *> m_recycled_order;
};

// ----------------------------------------------------------------------------

struct Hit {
    Hit() {}
    Hit(Direction h_, Direction v_):
        horizontal(h_), vertical(v_)
    {}
    Direction horizontal = k_direction_count;
    Direction vertical   = k_direction_count;
};

// --------------------------------- Helpers ----------------------------------

inline bool are_same(const Hit & lhs, const Hit & rhs)
    { return lhs.horizontal == rhs.horizontal && lhs.vertical == rhs.vertical; }

inline bool operator == (const Hit & lhs, const Hit & rhs) { return  are_same(lhs, rhs); }

inline bool operator != (const Hit & lhs, const Hit & rhs) { return !are_same(lhs, rhs); }

#if 0 // code disabled to check... BFS structure of code

// ----------------------------- level 0 helpers ------------------------------

// !I need tests!
/** @returns zero vector if there is no need for push */
std::tuple<Vector, Hit> find_min_push_displacement
    (const Rectangle &, const Rectangle & other, const Vector & displc);

std::vector<FullEntry *> prioritized_entries
    (EntryEntityRefMap &, std::vector<FullEntry *> &&);

Hit trim_displacement_for_barriers
    (const Rectangle &, Vector barriers, Vector & displacement);

Hit trim_displacement
    (const Rectangle &, const Rectangle & other, Vector & displc);

// much more intense written for growing or shrinking rectangles
bool trespass_occuring
    (const Rectangle &, const Rectangle & other, const Vector & displc);

Rectangle grow(Rectangle, const Size &);

Rectangle grow_by_displacement(Rectangle, const Vector & displc);

// ----------------------------- level 1 helpers ------------------------------

int large_displacement_step_count
    (const Rectangle &, const Rectangle & other, const Vector & displc);

std::tuple<Vector, Hit> find_min_push_displacement_small
    (const Rectangle &, const Rectangle & other, const Vector & displc);

Hit trim_small_displacement
    (const Rectangle &, const Rectangle & other, Vector & displc);

// ----------------------------- level 2 helpers ------------------------------

Hit values_from_displacement(const Vector &);

#endif
} // end of detail namespace -> into ::tdp

} // end of tdp namespace
