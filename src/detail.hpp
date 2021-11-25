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

#include "helpers.hpp"

#include <unordered_map> // incidentally included when compiled with g++
#include <unordered_set>

#include <cassert>

// Everything in this file is meant to contain implementation details that a
// client coder need not see
//
// however for the purposes of testing it's important to at some level reveal
// the code

namespace tdp {

namespace detail {

constexpr const int k_default_priority = -1;

// ----------------------------------------------------------------------------

struct FullEntry : tdp::Entry {
    // record only useful during a frame
    int priority = k_default_priority;
    // (I need some way to handle last appearance)
    bool first_appearance = true;

    // for swptry2
    Vector nudge;
    Real low_x, low_y, high_x, high_y;
};

inline void frame_reset(FullEntry & entry) {
    entry.priority = k_default_priority;
}

Rectangle get_grown_rectangle(const FullEntry &);

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

using EntryEntityRefMap = std::unordered_map<EntityRef, FullEntry, ecs::EntityHasher>;
using EntryMapView      = View<EntryEntityRefMap::iterator>;

// ----------------------------------------------------------------------------

class TdpHandlerEntryInformation : public TopDownPhysicsHandler {
public:
    const CollisionMatrix & collision_matrix() const final
        { return m_col_matrix; }

    void update_entry(const Entry & entry) final;

protected:
    EntryMapView entries_view()
        { return View{m_entries.begin(), m_entries.end()}; }

    auto entries_view() const
        { return View{m_entries.begin(), m_entries.end()}; }

    // intented to be called once per call to "run"
    // uh oh... this should never be called by any "TdpHandlerEntryInformation"
    // method
    void clean_up_containers();
#   if 0
    [[deprecated]] const FullEntry * find_entry(EntityRef ref) const {
        auto itr = m_entries.find(ref);
        if (itr == m_entries.end()) return nullptr;
        return &itr->second;
    }
#   endif
private:
    void set_collision_matrix_(CollisionMatrix &&) final;

    CollisionMatrix m_col_matrix;
    EntryEntityRefMap m_entries;
};

class TdpHandlerCollisionBehaviors : public TdpHandlerEntryInformation {
public:
    void run(EventHandler &) final;

protected:
    // iteration here
    void do_collision_work(EventHandler & handler);

    virtual void do_collision_work_on_entry(FullEntry &, EventHandler &) = 0;

    // should only be called by "do_collision_work_on_entry"
    void do_collision_work_for_pair(FullEntry & entry, const FullEntry & other_entry, EventHandler &);

    virtual void get_next_pushables_for_entry(FullEntry &, std::vector<PushPair> &) = 0;

    // should only be called by "get_next_pushables_for_entry"
    // possibly adds to the event recorder
    void add_if_pushable(FullEntry &, FullEntry &, std::vector<PushPair> &);

    void do_post_run(EventHandler &);

    // do whichever prep work (if any) before collision work begins in earnest
    virtual void prepare_for_collision_work() = 0;

private:
    void order_and_handle_pushes();

    template <typename Iter>
    [[nodiscard]] std::vector<PushPair> get_next_pushables
        (Iter beg, Iter end, std::vector<PushPair> && rv = std::vector<PushPair>());

    EventRecorder m_event_recorder;

    // recycled containers
    std::vector<PushPair> m_recycled_pushpairs_a, m_recycled_pushpairs_b;
    std::vector<FullEntry *> m_recycled_order;
};

// let's further break this class up
//
class TdpHandlerComplete final : public TdpHandlerCollisionBehaviors {
public:
    void prepare_for_collision_work() final;

private:
    void find_overlaps_(const Rectangle &, const OverlapInquiry &) const final;

    void order_and_handle_pushes();

    void do_collision_work_on_entry(FullEntry &, EventHandler &) final;

    void get_next_pushables_for_entry(FullEntry &, std::vector<PushPair> &) final;

    SpatialMapFront m_spatial_map;

    // ofc this means, each new entry will be asking for a few thousands of
    // instructions to allocate a map entry

    std::vector<EntrySpatialRef> m_entry_refs;
};

class QuadraticTdpHandler final : public TdpHandlerCollisionBehaviors {
public:
    void prepare_for_collision_work() final {}

private:
    void do_collision_work_on_entry(FullEntry & entry, EventHandler & handler) final {
        for (const auto & pair : entries_view()) {
            do_collision_work_for_pair(entry, pair.second, handler);
        }
    }

    void get_next_pushables_for_entry(FullEntry & entry, std::vector<PushPair> & rv) final {
        for (auto & pair : entries_view()) {
            add_if_pushable(entry, pair.second, rv);
        }
    }

    void find_overlaps_(const Rectangle &, const OverlapInquiry &) const final;
};

// --------------------------------- Helpers ----------------------------------

// code disabled to check... BFS structure of code
#ifdef MACRO_AABBTDP_SHOW_DETAILS_HELPERS

// ----------------------------- level 0 helpers ------------------------------
#if 0
// !I need tests!
/** @returns zero vector if there is no need for push */
std::tuple<Vector, HitSide> find_min_push_displacement
    (const Rectangle &, const Rectangle & other, const Vector & displc);
#endif
std::vector<FullEntry *> prioritized_entries
    (EntryMapView, std::vector<FullEntry *> &&);

std::vector<FullEntry *> prioritized_entries
    (EntryEntityRefMap &, std::vector<FullEntry *> &&);
#if 0
HitSide trim_displacement_for_barriers
    (const Rectangle &, Vector barriers, Vector & displacement);

HitSide trim_displacement
    (const Rectangle &, const Rectangle & other, Vector & displc);

// much more intense written for growing or shrinking rectangles
bool trespass_occuring
    (const Rectangle &, const Rectangle & other, const Vector & displc);

Rectangle grow(Rectangle, const Size &);

Rectangle grow_by_displacement(Rectangle, const Vector & displc);

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
#endif
} // end of detail namespace -> into ::tdp

} // end of tdp namespace
