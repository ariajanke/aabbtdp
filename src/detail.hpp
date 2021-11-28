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

constexpr const int k_default_priority = 0;

// ----------------------------------------------------------------------------

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

inline void update_broad_boundries(
    std::vector<FullEntry *>::iterator beg,
    std::vector<FullEntry *>::iterator end)
{
    update_broad_boundries(beg, end,
        [](std::vector<FullEntry *>::iterator itr) -> FullEntry & { return **itr; });
}

inline void check_wall_collision(FullEntry & entry, EventRecorder & recorder) {
    Vector barrier = find_barrier_for_displacement
        (entry.displacement, entry.positive_barrier, entry.negative_barrier);
    if (trim_displacement_for_barriers(entry.bounds, barrier, entry.displacement) != HitSide()) {
        recorder.emplace_event(entry.entity, EntityRef(), false);
    }
}

inline void check_collision_on(FullEntry & entry, const FullEntry & other_entry,
                        const CollisionMatrix & col_mat, EventRecorder & recorder,
                        EventHandler & handler)
{
    using namespace tdp::interaction_classes;
    switch (col_mat(entry.collision_layer, other_entry.collision_layer)) {
    case k_as_solid: {
        auto gv = trim_displacement(entry.bounds, other_entry.bounds, entry.displacement);
        if (gv == HitSide()) return;
        recorder.emplace_event(entry.entity, other_entry.entity, false);
        break;
    }
    case k_as_trespass: {
        bool first_appearance_overlap = entry.first_appearance && overlaps(entry.bounds, other_entry.bounds);
        bool regular_trespass         = trespass_occuring(entry.bounds, other_entry.bounds, entry.displacement);
        if (!first_appearance_overlap && !regular_trespass) return;
        // this is an "uh-oh" moment if we're reusing containers
        handler.on_trespass(entry.entity, other_entry.entity);
        break;
    }
    default: break;
    }
}

inline void finalize_entry(FullEntry & entry, EventHandler & handler) {
    // fine with other bounds being finalized for trespass events
    set_top_left_of(entry.bounds, top_left_of(entry.bounds) + entry.displacement);
    if (entry.growth != Size()) {
        entry.bounds = grow(entry.bounds, entry.growth);
    }
    // this is fine here, not reusing containers at this point, unlike
    // while being in the for loop above
    handler.finalize_entry(entry.entity, entry.bounds);
}

inline void frame_reset(FullEntry & entry) {
    entry.priority = k_default_priority;
}

Rectangle get_grown_rectangle(const FullEntry &);

// ----------------------------------------------------------------------------

using EntryEntityRefMap = std::unordered_map<EntityRef, FullEntry, ecs::EntityHasher>;
using EntryMapView      = View<EntryEntityRefMap::iterator>;

// ----------------------------------------------------------------------------

class TdpHandlerEntryInformation final {
public:
    const CollisionMatrix & collision_matrix() const
        { return m_col_matrix; }

    void update_entry(const Entry & entry) ;

    void set_collision_matrix_(CollisionMatrix &&) ;

    EntryMapView entries_view()
        { return View{m_entries.begin(), m_entries.end()}; }

    auto entries_view() const
        { return View{m_entries.begin(), m_entries.end()}; }

    // intented to be called once per call to "run"
    // uh oh... this should never be called by any "TdpHandlerEntryInformation"
    // method
    void clean_up_containers();

    const FullEntry * find_entry(EntityRef eref) const {
        auto itr = m_entries.find(eref);
        return itr == m_entries.end() ? nullptr : &itr->second;
    }

    void absorb_nudges() {
        tdp::detail::absorb_nudges(m_entries.begin(), m_entries.end(),
            [](auto itr) -> FullEntry & { return itr->second; });
    }

private:
    CollisionMatrix m_col_matrix;
    EntryEntityRefMap m_entries;
};

// --------------------------------- Helpers ----------------------------------

class IterationBase {
public:
    struct SequenceInterface {
        virtual ~SequenceInterface() {}
        virtual void prestep(FullEntry &) = 0;
        virtual void step(FullEntry &, FullEntry & other_entry) = 0;
        virtual void poststep(FullEntry &) = 0;
    };

    virtual ~IterationBase() {}

    virtual void for_each_sequence(SequenceInterface &) = 0;

    template <typename OnPairWise>
    void for_each(OnPairWise && do_pair_wise);
};

template <typename OnPairWise>
void IterationBase::for_each(OnPairWise && do_pair_wise) {
    class Impl final : public SequenceInterface {
    public:
        Impl(OnPairWise && do_pair_wise): m_f(std::move(do_pair_wise)) {}
        void prestep(FullEntry &) final {}
        void step(FullEntry & entry, FullEntry & other_entry) final
            { m_f(entry, other_entry); }
        void poststep(FullEntry &) final {}

    private:
        OnPairWise m_f;
    };
    Impl impl(std::move(do_pair_wise));
    for_each_sequence(impl);
}

void do_collision_work
    (EventHandler &, IterationBase &, const CollisionMatrix &,
     EventRecorder &, TdpHandlerEntryInformation &);

} // end of detail namespace -> into ::tdp

} // end of tdp namespace
