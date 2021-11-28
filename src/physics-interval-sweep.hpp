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

#include "detail.hpp"

namespace tdp {

namespace detail {

// another exciting implementation may use a bit-matrix
// and we select pairs that way...
class SweepContainer final : public IterationBase {
public:
    enum Cand { k_x_wise, k_y_wise, k_both };

    template <typename Iter, typename IterToPointer>
    void populate(Iter beg, Iter end, IterToPointer && to_pointer) {
        m_reorder.clear();
        for (auto itr = beg; itr != end; ++itr) {
            m_reorder.push_back(to_pointer(*itr));
        }
    }

    void for_each_sequence(SequenceInterface & intf) final;

    // something to test extensively :)
    static constexpr const int  k_use_quadratic_thershold = 20;

private:
    static constexpr const Real k_adjust_amount           = 0.;

    using WsIter = std::vector<FullEntry *>::iterator;

    // bounds should be expanded by nudge and displacement?

    auto make_in_range_x(WsIter itr) {
        assert(itr >= m_reorder.begin() && itr < m_reorder.end());
        Real x_end = (**itr).high_x;
        return [x_end, this] (decltype(itr) jtr) {
            assert(jtr >= m_reorder.begin() && jtr <= m_reorder.end());
            if (jtr == m_reorder.end()) return false;
            return (**jtr).low_x < x_end;
        };
    }

    auto make_in_range_y(WsIter jtr) {
        assert(jtr >= m_reorder.begin() && jtr < m_reorder.end());
        Real y_end = (**jtr).high_y;
        return [y_end, this] (decltype(jtr) ktr) {
            assert(ktr >= m_reorder.begin() && ktr <= m_reorder.end());
            if (ktr == m_reorder.end()) return false;
            return (**ktr).low_y < y_end;
        };
    }

    static bool order_entries_horizontally(FullEntry * lhs, FullEntry * rhs)
        { return lhs->low_x < rhs->low_x; }

    static bool order_entries_vertically(FullEntry * lhs, FullEntry * rhs)
        { return lhs->low_y < rhs->low_y; }

    void sweep_x_wise(SequenceInterface &);

    void sweep_y_wise(SequenceInterface &);

    std::vector<FullEntry *> m_reorder;
};

class IntervalSweepHandler final : public SweepSwitchPhysicsHandler {
public:
    void run(EventHandler &) final;

    void check_to_switch_axis() final {}

    const CollisionMatrix & collision_matrix() const final
        { return m_info.collision_matrix(); }

    void update_entry(const Entry & entry) final
        { m_info.update_entry(entry); }

    const int * get_push_level_for(EntityRef eref) const {
        auto ptr = m_info.find_entry(eref);
        return ptr ? &ptr->priority : nullptr;
    }

private:
    void set_collision_matrix_(CollisionMatrix && colmat) final
        { m_info.set_collision_matrix_(std::move(colmat)); }

    void find_overlaps_(const Rectangle &, const OverlapInquiry &) const final;

    EventRecorder m_event_recorder;
    SweepContainer m_workspace;
    TdpHandlerEntryInformation m_info;
};

} // end of detail namespace -> into ::tdp

} // end of tdp namespace
