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
#ifdef MACRO_AABBTDP_LIBRARY_BUILD_FOR_PERSONAL_ECS_REFERENCE
#   include <ecs/ecs.hpp>
#endif

namespace {

using namespace cul::exceptions_abbr;
static constexpr const auto k_x_wise = tdp::SweepDirection::x_wise;
static constexpr const auto k_y_wise = tdp::SweepDirection::y_wise;

} // end of <anonymous> namespace

namespace tdp {

void SweepContainer::for_each_sequence(SequenceInterface & intf) {
    update_broad_boundries(m_reorder.begin(), m_reorder.end());
    for_each_sequence_(intf);
}

// ----------------------------------------------------------------------------

void IntervalSweepHandler::check_to_switch_axis() {
    auto x_is_less = count_sweep_along_axis(k_x_wise) < count_sweep_along_axis(k_y_wise);
    m_workspace = choose_by_direction(x_is_less ? k_x_wise : k_y_wise);
}

int IntervalSweepHandler::count_sweep_along_axis(SweepDirection direction) {
    auto sweep_cont = choose_by_direction(direction);
    populate_sweep_container(*sweep_cont, entries_view());
    return sweep_cont->count_sweep();
}

/* private */ void IntervalSweepHandler::prepare_iteration
    (CollisionWorker & do_collision_work, EventHandler & event_handler)
{
    populate_sweep_container(*m_workspace, entries_view());
    do_collision_work(event_handler, *m_workspace);
}

/* private */ SweepContainer * IntervalSweepHandler::choose_by_direction
    (SweepDirection direction)
{
    switch (direction) {
    case k_x_wise: return &m_sweep_x_cont;
    case k_y_wise: return &m_sweep_y_cont;
    default: throw InvArg("IntervalSweepHandler::choose_by_direction: "
                          "direction has an invalid value (corrupt data?).");
    }
}

/* private static */ void IntervalSweepHandler::populate_sweep_container
    (SweepContainer & sweep_container, EntryMapView entries_view)
{
    using PairType = EntryEntityRefMap::value_type;
    sweep_container.populate(entries_view.begin(), entries_view.end(),
                             [](PairType & pt) { return &pt.second; });
}

/* private */ void IntervalSweepHandler::find_overlaps_(const Rectangle & rect, const OverlapInquiry & inq) const {
    for (const auto & pair : entries_view()) {
        if (cul::find_rectangle_intersection(rect, pair.second.bounds).width > 0)
            inq(pair.second);
    }
}

} // end of tdp namespace
