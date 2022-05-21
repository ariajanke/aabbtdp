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
#include "CollisionHandler.hpp"

namespace tdp {

enum class SweepDirection { x_wise, y_wise };

// another exciting implementation may use a bit-matrix
// and we select pairs that way...
class SweepContainer : public IterationBase {
public:

    template <typename Iter, typename IterToPointer>
    void populate(Iter beg, Iter end, IterToPointer && to_pointer) {
        m_reorder.clear();
        for (auto itr = beg; itr != end; ++itr) {
            m_reorder.push_back(to_pointer(*itr));
        }
    }

    void for_each_sequence(SequenceInterface & intf) final;

    // still a O(n log n)... as it sorts the population also
    virtual int count_sweep() = 0;

    // something to test extensively :)
    static constexpr const int  k_use_quadratic_thershold = 20;

    static Real low_x (const FullEntry & fe) { return fe.board_bounds.low_x ; }
    static Real low_y (const FullEntry & fe) { return fe.board_bounds.low_y ; }
    static Real high_x(const FullEntry & fe) { return fe.board_bounds.high_x; }
    static Real high_y(const FullEntry & fe) { return fe.board_bounds.high_y; }

protected:
    using WsIter = std::vector<FullEntry *>::iterator;

    WsIter begin_() { return m_reorder.begin(); }

    WsIter end_() { return m_reorder.end(); }

    template <typename F>
    void update_broad_boundries(F && func) {
#       if 0
        // f must be...
        struct Impl final {
            explicit Impl(F && f_): m_f(std::move(f_)) {}

            BoardBoundries operator () (const FullEntry & fe) const
                { return m_f(fe); }
        private:
            F m_f;
        };
        Impl impl{std::move(func)};
#       endif
        for (auto itr = m_reorder.begin(); itr != m_reorder.end(); ++itr)
            { (**itr).board_bounds = func(**itr); }
    }

    virtual void for_each_sequence_(SequenceInterface &) = 0;

private:
    // bounds should be expanded by nudge and displacement?
    std::vector<FullEntry *> m_reorder;
};

// design note: do I trade readability for templates OR
//                         violate DRY?
template <Real(*low_i)(const FullEntry &), Real(*high_i)(const FullEntry &),
          Real(*low_j)(const FullEntry &), Real(*high_j)(const FullEntry &)>
class SweepIJContainer final : public SweepContainer {
public:
    int count_sweep() final;

private:
    void for_each_sequence_(SequenceInterface & intf) final;

    static bool order_entries(FullEntry * lhs, FullEntry * rhs)
        { return low_i(*lhs) < low_i(*rhs); }

    WsIter get_i_wise_end(WsIter itr);

    template <typename Func>
    void for_i_intervals(Func && f);
};

using SweepXContainer = SweepIJContainer<
    SweepContainer::low_x, SweepContainer::high_x,
    SweepContainer::low_y, SweepContainer::high_y>;

using SweepYContainer = SweepIJContainer<
    SweepContainer::low_y, SweepContainer::high_y,
    SweepContainer::low_x, SweepContainer::high_x>;

// ----------------------------------------------------------------------------

class IntervalSweepHandler final :
    public SweepSwitchPhysicsHandler, public CollisionHandler
{
public:
    void check_to_switch_axis() final;

    int count_sweep_along_axis(SweepDirection direction);

private:
    void find_overlaps_(const Rectangle &, const OverlapInquiry &) const final;

    void prepare_iteration(CollisionWorker &, EventHandler &) final;

    SweepContainer * choose_by_direction(SweepDirection direction);

    static void populate_sweep_container(SweepContainer &, EntryMapView);

    SweepXContainer m_sweep_x_cont;
    SweepYContainer m_sweep_y_cont;
    SweepContainer * m_workspace = &m_sweep_x_cont;
};

// ----------------------------------------------------------------------------

template <Real(*low_i)(const FullEntry &), Real(*high_i)(const FullEntry &),
          Real(*low_j)(const FullEntry &), Real(*high_j)(const FullEntry &)>
int SweepIJContainer<low_i, high_i, low_j, high_j>::count_sweep() {
    int sum = 0;
    for_i_intervals([&sum](WsIter, WsIter jtr, WsIter jtr_end) {
        sum += (jtr_end - jtr);
    });
    return sum;
}

template <Real(*low_i)(const FullEntry &), Real(*high_i)(const FullEntry &),
          Real(*low_j)(const FullEntry &), Real(*high_j)(const FullEntry &)>
/* private */ void SweepIJContainer<low_i, high_i, low_j, high_j>::for_each_sequence_
    (SequenceInterface & intf)
{
    // moved from a caller
    using Fp = BoardBoundries (*)(const FullEntry &);
    auto fp = Fp(compute_board_boundries);
    update_broad_boundries(fp);

    // processing entries before prestep is fine
    //
    // I still need a test case!
    for_i_intervals([&intf](WsIter itr, WsIter beg, WsIter end) {
        intf.prestep(**itr);
        auto intersects_j_wise = [itr](const FullEntry & other)
            { return high_j(**itr) > low_j(other) && high_j(other) > low_j(**itr); };
        for (auto jtr = beg; jtr != end; ++jtr) {
            // anymore checks and I worry we're hitting brute force again
            // second part of the overlap feature
            if (intersects_j_wise(**jtr)) {
                intf.step(**itr, **jtr);
            }
            assert(*jtr != *itr);
        }
        intf.poststep(**itr);

        // moving pairs to post finalization was easier than I thought
        for (auto jtr = beg; jtr != end; ++jtr) {
            if (intersects_j_wise(**jtr)) intf.step(**jtr, **itr);
        }
    });
}

template <Real(*low_i)(const FullEntry &), Real(*high_i)(const FullEntry &),
          Real(*low_j)(const FullEntry &), Real(*high_j)(const FullEntry &)>
/* private */
    typename SweepIJContainer<low_i, high_i, low_j, high_j>::WsIter
    SweepIJContainer<low_i, high_i, low_j, high_j>::get_i_wise_end
    (WsIter itr)
{
    auto make_in_range_i = [this](WsIter itr) {
        assert(itr >= begin_() && itr < end_());
        Real i_end = high_i(**itr);
        return [i_end, this] (decltype(itr) jtr) {
            assert(jtr >= begin_() && jtr <= end_());
            if (jtr == end_()) return false;
            return low_i(**jtr) < i_end;
        };
    };
    auto itr_end = itr;
    for (auto in_range_i = make_in_range_i(itr); in_range_i(itr_end); ++itr_end) {}
    return itr_end;
}

template <Real(*low_i)(const FullEntry &), Real(*high_i)(const FullEntry &),
          Real(*low_j)(const FullEntry &), Real(*high_j)(const FullEntry &)>
template <typename Func>
/* private */ void SweepIJContainer<low_i, high_i, low_j, high_j>::for_i_intervals
    (Func && f)
{
    // a nasty side effect?
    sort(begin_(), end_(), order_entries);
    for (auto itr = begin_(); itr != end_(); ++itr) {
        auto itr_end = get_i_wise_end(itr);

        // we don't want itr to be changed...
        f(itr, (itr == itr_end) ? itr : itr + 1, itr_end);
    }
}

} // end of tdp namespace
