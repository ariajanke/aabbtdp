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
class SweepContainer final {
public:
    template <typename ... Types>
    using Tuple = std::tuple<Types...>;

    struct SeqInterface {
        virtual ~SeqInterface() {}
        virtual void prestep(FullEntry &) = 0;
        virtual void step(FullEntry &, FullEntry & other_entry) = 0;
        virtual void poststep(FullEntry &) = 0;

        // debug sort of thing
        virtual void post_glob_rectangle(const Rectangle &) {}
    };
    enum Cand { k_x_wise, k_y_wise, k_both };


    template <typename Iter, typename IterToPointer>
    void populate(Iter beg, Iter end, IterToPointer && to_pointer) {
        m_reorder_x.clear();
        for (auto itr = beg; itr != end; ++itr) {
            m_reorder_x.push_back(to_pointer(*itr));
        }
    }

    template <typename OnPairWise>
    void for_each(OnPairWise && do_pair_wise);

    template <typename OnPairWise, typename PostGlob>
    void for_each(OnPairWise && do_pair_wise, PostGlob && post_glob);

    void for_each_sequence(SeqInterface & intf);

private:
    static constexpr const int  k_next_dim_minimum = 12;
    static constexpr const Real k_adjust_amount    = 0.;

    using WsIter = std::vector<FullEntry *>::iterator;

    // bounds should be expanded by nudge and displacement?

    auto make_in_range_x(WsIter itr) {
        assert(itr >= m_reorder_x.begin() && itr < m_reorder_x.end());
        Real x_end = high_x(**itr);
        return [x_end, this] (decltype(itr) jtr) {
            assert(jtr >= m_reorder_x.begin() && jtr <= m_reorder_x.end());
            if (jtr == m_reorder_x.end()) return false;
            return low_x(**jtr) < x_end;
        };
    }

    auto make_in_range_y(WsIter jtr) {
        assert(   (jtr >= m_reorder_y.begin() && jtr < m_reorder_y.end())
               || (jtr >= m_reorder_x.begin() && jtr < m_reorder_x.end()));
        Real y_end = high_y(**jtr);
        return [y_end, this] (decltype(jtr) ktr) {
            assert(ktr >= m_reorder_y.begin() && ktr <= m_reorder_y.end());
            if (ktr == m_reorder_y.end()) return false;
            return low_y(**ktr) < y_end;
        };
    }

    static Vector full_displacement(const FullEntry & fe)
        { return fe.displacement + fe.nudge; }
public:
    static Real low_x(const FullEntry & fe) {
        auto dx = full_displacement(fe).x;
        return fe.bounds.left + ((dx < 0) ? dx : 0) - k_adjust_amount;
    }

    static Real high_x(const FullEntry & fe) {
        auto dx = full_displacement(fe).x;
        return cul::right_of(fe.bounds) + ((dx > 0) ? dx : 0) + k_adjust_amount;
    }

    static Real low_y(const FullEntry & fe) {
        auto dy = full_displacement(fe).y;
        return fe.bounds.top + ((dy < 0) ? dy : 0) - k_adjust_amount;
    }

    static Real high_y(const FullEntry & fe) {
        auto dy = full_displacement(fe).y;
        return cul::bottom_of(fe.bounds) + ((dy > 0) ? dy : 0) + k_adjust_amount;
    }

private:
    static bool order_entries_horizontally(FullEntry * lhs, FullEntry * rhs) {
        return   lhs->low_x //lhs->bounds.left// + lhs->nudge.x
               < rhs->low_x; //rhs->bounds.left;// + rhs->nudge.x;
    }

    static bool order_entries_vertically(FullEntry * lhs, FullEntry * rhs) {
        return   lhs->low_y// lhs->bounds.top// + lhs->nudge.y
               < rhs->low_y;// rhs->bounds.top;// + rhs->nudge.y;
    }

    std::vector<FullEntry *> m_reorder_x, m_reorder_y;
};

template <typename OnPairWise>
void SweepContainer::for_each(OnPairWise && do_pair_wise) {
    class Impl final : public SeqInterface {
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

template <typename OnPairWise, typename PostGlob>
void SweepContainer::for_each(OnPairWise && do_pair_wise, PostGlob && post_glob) {
    class Impl final : public SeqInterface {
    public:
        Impl(OnPairWise && do_pair_wise, PostGlob && post_glob):
            m_f(std::move(do_pair_wise)), m_pg(std::move(post_glob)) {}
        void prestep(FullEntry &) final {}
        void step(FullEntry & entry, FullEntry & other_entry) final
            { m_f(entry, other_entry); }
        void poststep(FullEntry &) final {}
        void post_glob_rectangle(const Rectangle & rect) final
            { m_pg(rect); }

    private:
        OnPairWise m_f;
        PostGlob m_pg;
    };
    Impl impl(std::move(do_pair_wise), std::move(post_glob));
    for_each_sequence(impl);
}

class IntervalSweepHandler final : public TdpHandlerEntryInformation {
public:
    void run(EventHandler &) final;

private:
    void find_overlaps_(const Rectangle &, const OverlapInquiry &) const final;

    EventRecorder m_event_recorder;
    SweepContainer m_workspace;
};

} // end of detail namespace -> into ::tdp

} // end of tdp namespace
