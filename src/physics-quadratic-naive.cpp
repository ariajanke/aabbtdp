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

#include "physics-quadratic-naive.hpp"

namespace {

using tdp::detail::IterationBase, tdp::detail::EntryMapView;

class QuadraticIteration final : public  IterationBase {
public:
    QuadraticIteration(EntryMapView && emv): m_view(std::move(emv)) {}

    void for_each_sequence(SequenceInterface & intf) final {
        for (auto & pair : m_view) {
            intf.prestep(pair.second);
            for (auto & other_pair : m_view) {
                if (pair.first == other_pair.first) continue;
                intf.step(pair.second, other_pair.second);
            }
            intf.poststep(pair.second);
        }
    }

private:
    EntryMapView m_view;
};

} // end of <anonymous> namespace

namespace tdp {

namespace detail {

void Quadratic2DPhysicsImpl::run(EventHandler & event_handler) {
    m_info.clean_up_containers();

    QuadraticIteration qi{m_info.entries_view()};

    // still quite a few dupelications...

    do_collision_work(event_handler, qi, collision_matrix(), m_event_recorder, m_info);

    m_event_recorder.send_events(event_handler);
}

/* private */ void Quadratic2DPhysicsImpl::find_overlaps_
    (const Rectangle & rect, const OverlapInquiry & inq) const
{
    for (const auto & pair : m_info.entries_view()) {
        if (cul::find_rectangle_intersection(rect, pair.second.bounds).width > 0)
            inq(pair.second);
    }
}

} // end of detail namespace -> into ::tdp

} // end of tdp namespace
