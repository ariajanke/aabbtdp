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

namespace tdp {

namespace detail {

void SweepContainer::for_each_sequence(SequenceInterface & intf) {
    update_broad_boundries(m_reorder.begin(), m_reorder.end());

    for (auto & feptr : m_reorder) {
        assert(feptr);
        intf.prestep(*feptr);
    }

    //thread_local static int i = 0;
    //auto fptr = i++ % 2 == 0 ? &SweepContainer::sweep_x_wise : &SweepContainer::sweep_y_wise;
    //((*this).*fptr)(intf);
    sweep_y_wise(intf);
}

/* private */ void SweepContainer::sweep_x_wise(SequenceInterface & intf) {
    sort(m_reorder.begin(), m_reorder.end(), order_entries_horizontally);
    for (auto itr = m_reorder.begin(); itr != m_reorder.end(); ++itr) {
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
                if ((**itr).high_y > (**jtr).low_y && (**jtr).high_y > (**itr).low_y) {
                    intf.step(**itr, **jtr);
                    intf.step(**jtr, **itr);
                }
                assert(*jtr != *itr);
            }
        }
        intf.poststep(**itr);
    }
}

/* private */ void SweepContainer::sweep_y_wise(SequenceInterface & intf) {
    sort(m_reorder.begin(), m_reorder.end(), order_entries_vertically);
    for (auto itr = m_reorder.begin(); itr != m_reorder.end(); ++itr) {
        // seek **itr y-wise... O(log n)
        // we now have two sequences
        // the intersection is our interest

        auto itr_end = itr;
        for (auto in_range_y = make_in_range_y(itr); in_range_y(itr_end); ++itr_end) {}

        if (itr == itr_end) {
            // do nothing
        } else {
            for (auto jtr = itr + 1; jtr != itr_end; ++jtr) {
                // anymore and I worry we're hitting brute force again
                // second part of the overlap feature
                if ((**itr).high_x > (**jtr).low_x && (**jtr).high_x > (**itr).low_x) {
                    intf.step(**itr, **jtr);
                    intf.step(**jtr, **itr);
                }
                assert(*jtr != *itr);
            }
        }
        intf.poststep(**itr);
    }
}

// ----------------------------------------------------------------------------

/* private */ void IntervalSweepHandler::prepare_iteration
    (CollisionWorker & do_collision_work, EventHandler & event_handler)
{
    using PairType = EntryEntityRefMap::value_type;
    m_workspace.populate(entries_view().begin(), entries_view().end(),
                         [](PairType & pt) { return &pt.second; });
    do_collision_work(event_handler, m_workspace);
}

/* private */ void IntervalSweepHandler::find_overlaps_(const Rectangle & rect, const OverlapInquiry & inq) const {
    for (const auto & pair : entries_view()) {
        if (cul::find_rectangle_intersection(rect, pair.second.bounds).width > 0)
            inq(pair.second);
    }
}

} // end of detail namespace -> into ::tdp

} // end of tdp namespace
