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

#include "physics-interval-sweep.hpp"

namespace {

using SeqInterface = tdp::detail::SweepContainer::SeqInterface;
using tdp::detail::EventRecorder, tdp::EventHandler, tdp::CollisionMatrix,
      tdp::detail::FullEntry, tdp::Vector, tdp::Size, tdp::detail::HitSide,
      ecs::EntityRef, tdp::Rectangle, tdp::detail::find_min_push_displacement,
      tdp::detail::find_barrier_for_displacement,
      tdp::detail::trim_displacement_for_barriers,
      tdp::detail::trim_displacement, tdp::detail::trespass_occuring,
      tdp::detail::grow;

template <typename ... Types>
using Tuple = std::tuple<Types...>;

class ColWorkImpl final : public SeqInterface {
public:
    explicit ColWorkImpl
        (int p, EventRecorder & recorder, EventHandler & handler,
         const CollisionMatrix & col_mat):
        m_priority(p), m_event_recorder(recorder), m_event_handler(handler),
        m_col_matrix(col_mat) {}

    // all of this is copy-pasta code...
    void prestep(FullEntry & entry) final {
        if (entry.priority != m_priority) return;
        Vector barrier = find_barrier_for_displacement
            (entry.displacement, entry.positive_barrier, entry.negative_barrier);
        if (trim_displacement_for_barriers(entry.bounds, barrier, entry.displacement) != HitSide()) {
            m_event_recorder.emplace_event(entry.entity, EntityRef(), false);
        }
    }

    void step(FullEntry & entry, FullEntry & other_entry) final {
        if (entry.priority != m_priority) return;
        using namespace tdp::interaction_classes;
        switch (m_col_matrix(entry.collision_layer, other_entry.collision_layer)) {
        case k_as_solid: {
            auto gv = trim_displacement(entry.bounds, other_entry.bounds, entry.displacement);
            if (gv == HitSide()) return;
            m_event_recorder.emplace_event(entry.entity, other_entry.entity, false);
            break;
        }
        case k_as_trespass: {
            bool first_appearance_overlap = entry.first_appearance && overlaps(entry.bounds, other_entry.bounds);
            bool regular_trespass         = trespass_occuring(entry.bounds, other_entry.bounds, entry.displacement);
            if (!first_appearance_overlap && !regular_trespass) return;
            // this is an "uh-oh" moment if we're reusing containers
            m_event_handler.on_trespass(entry.entity, other_entry.entity);
            break;
        }
        default: break;
        }
    }

    void poststep(FullEntry & entry) final {
        if (entry.priority != m_priority) return;
        // fine with other bounds being finalized for trespass events
        set_top_left_of(entry.bounds, top_left_of(entry.bounds) + entry.displacement);
        if (entry.growth != Size()) {
            entry.bounds = grow(entry.bounds, entry.growth);
        }
        // this is fine here, not reusing containers at this point, unlike
        // while being in the for loop above
        m_event_handler.finalize_entry(entry.entity, entry.bounds);
    }

    void post_glob_rectangle(const Rectangle &) {

    }

private:
    int m_priority;
    EventRecorder & m_event_recorder;
    EventHandler & m_event_handler;
    const CollisionMatrix & m_col_matrix;

};

} // end of <anonymous> namespace

namespace tdp {

namespace detail {

#if 0

#else

Tuple<FullEntry *, FullEntry *> make_entry_tuple(FullEntry * a, FullEntry * b) {
    using std::min, std::max, std::make_tuple;
    return make_tuple(min(a, b), max(a, b));
}

template <typename T>
T * nullptr_if(bool b, T * ptr) {
    return reinterpret_cast<T *>(std::size_t(!b)*reinterpret_cast<std::size_t>(ptr));
};

void SweepContainer::for_each_sequence(SeqInterface & intf) {
    for (auto & feptr : m_reorder_x) {
        assert(feptr);
        feptr->low_y  = low_y (*feptr);
        feptr->high_y = high_y(*feptr);
        feptr->low_x  = low_x (*feptr);
        feptr->high_x = high_x(*feptr);
        intf.prestep(*feptr);
    }

    // There are a whole lot of ways to implement this sort of thing c:
    using std::lower_bound, std::sort;
#   if 0
    m_reorder_y.clear();
    m_reorder_y.insert(m_reorder_y.begin(), m_reorder_x.begin(), m_reorder_x.end());
    assert(m_reorder_x.size() == m_reorder_y.size());
#   endif

    //std::thread{ [&] {
    sort(m_reorder_x.begin(), m_reorder_x.end(), order_entries_horizontally);
    for (auto itr = m_reorder_x.begin(); itr != m_reorder_x.end(); ++itr) {
        // seek **itr y-wise... O(log n)
        // we now have two sequences
        // the intersection is our interest

        auto itr_end = itr;
        for (auto in_range_x = make_in_range_x(itr); in_range_x(itr_end); ++itr_end) {}

        if (itr == itr_end) {
            // do nothing
        } else {
            for (auto jtr = itr + 1; jtr != itr_end; ++jtr) {
                // anymore and I worry we're hitting brute force again
                // second part of the overlap feature
                const auto & a = (**itr);
                const auto & b = (**jtr);
                bool pass = !( a.high_y > b.low_y && b.high_y > a.low_y ); //a.high_y <= b.low_y;
                if (!pass) {
                    intf.step(**itr, **jtr);
                    intf.step(**jtr, **itr);
                }
                assert(*jtr != *itr);
            }
        }
        intf.poststep(**itr);
    }
    //} }.join();
#   if 0
    //std::thread{ [&] {
    sort(m_reorder_y.begin(), m_reorder_y.end(), order_entries_vertically);
    for (auto itr = m_reorder_y.begin(); itr != m_reorder_y.end(); ++itr) {
        auto itr_end = itr;
        for (auto in_range_y = make_in_range_y(itr); in_range_y(itr_end); ++itr_end) {}

        if (itr == itr_end) {
            // do nothing
        } else {
            for (auto jtr = itr + 1; jtr != itr_end; ++jtr) {
#               if 0
                if ((**itr).entity == m_debug_ent)
                    m_cands.emplace_back(k_y_wise, (**jtr).bounds, (**jtr).entity);
                if ((**jtr).entity == m_debug_ent)
                    m_cands.emplace_back(k_y_wise, (**itr).bounds, (**itr).entity);
#               endif
                assert(*jtr != *itr);
                m_entry_pairs_y.push_back(make_entry_tuple(*itr, *jtr));
            }

        }
    }
    //} }.join();
#   endif
}
#endif

void IntervalSweepHandler::run(EventHandler & event_handler) {
    clean_up_containers();

    using PairType = EntryEntityRefMap::value_type;
    using std::get;
    // we want to process pushes first, in this case
    // the most limiting pushes
    for (auto & pair : entries_view()) {
        pair.second.priority = 0;
        if (pair.second.displacement.x < 0) {
            int k = 0;
            ++k;
        }
    }

    m_workspace.populate(entries_view().begin(), entries_view().end(),
                         [](PairType & pt) { return &pt.second; }
    );

    // push ordering...
    // note: there still maybe cases unaccounted for
    static constexpr const int k_max_iterations = 128;
    int iteration = 1;
    for (bool should_check_more = true; should_check_more && iteration < k_max_iterations; ++iteration) {
        should_check_more = false;
        // have to keep doing the whole thing over and over again...
        m_workspace.for_each([this, &iteration, &should_check_more](FullEntry & entry, FullEntry & other_entry) {
            if (entry.displacement == Vector{} && entry.nudge == Vector{}) return;

            // is other_entry pushable?
            auto col_class = collision_matrix()(entry.collision_layer, other_entry.collision_layer);
            if (col_class != InteractionClass::k_as_solid || !other_entry.pushable) return;

            // find push displacement
            auto gv = find_min_push_displacement(entry.bounds,
                displace(other_entry.bounds, other_entry.displacement + other_entry.nudge),
                entry.displacement + entry.nudge);
            if (get<Vector>(gv) == Vector{}) return;

            // The pushed entity should move with a higher priority
            other_entry.priority = iteration;
            should_check_more = true;
            // nudges...?
            other_entry.nudge += get<Vector>(gv);
            m_event_recorder.emplace_event(other_entry.entity, entry.entity, true);
        });
    }

    for (auto & pair : entries_view()) {
        pair.second.displacement += pair.second.nudge;
        pair.second.nudge = Vector{};
    }

    // start finalizing
    assert(iteration > 0);
    for (; iteration; --iteration) {
        // process priority iteration - 1
        ColWorkImpl impl(iteration - 1, m_event_recorder, event_handler, collision_matrix());
        m_workspace.for_each_sequence(impl);
    }

    m_event_recorder.send_events(event_handler);
    for (auto & pair : entries_view()) {
        pair.second.first_appearance = false;
    }
}

void IntervalSweepHandler::find_overlaps_(const Rectangle & rect, const OverlapInquiry & inq) const {
    for (const auto & pair : entries_view()) {
        if (cul::find_rectangle_intersection(rect, pair.second.bounds).width > 0)
            inq(pair.second);
    }
}


} // end of detail namespace -> into ::tdp

} // end of tdp namespace
