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

#include "physics-grid.hpp"
#ifdef MACRO_AABBTDP_LIBRARY_BUILD_FOR_PERSONAL_ECS_REFERENCE
#   include <ecs/ecs.hpp>
#endif

namespace {

using std::get, tdp::FullEntry, cul::convert_to,
      tdp::Tuple, cul::right_of, cul::bottom_of, std::floor, cul::is_real;
using namespace cul::exceptions_abbr;

} // end of <anonymous> namespace

namespace tdp {

VectorI find_rectangle_start
    (Real low_x, Real low_y, const Size & cell_size, const Vector & offset)
{
    assert(is_real(cell_size) && is_real(offset));
    assert(is_real(low_x) && is_real(low_y));
    return VectorI{int(floor((low_x - offset.x) / cell_size.width )),
                   int(floor((low_y - offset.y) / cell_size.height))};
}

RectangleI find_rectangle_range
    (Real low_x, Real low_y, Real high_x, Real high_y,
     const Size & cell_size, const Vector & offset)
{
    assert(cell_size.width >= 0 && cell_size.height >= 0);
    assert(is_real(cell_size) && is_real(offset));
    assert(is_real(low_x) && is_real(low_y) && is_real(high_x) && is_real(high_y));
    assert(high_x >= low_x && high_y >= low_y);

    if (cell_size.width == 0 || cell_size.height == 0) return RectangleI{};

    auto start = find_rectangle_start(low_x, low_y, cell_size, offset);
    auto end   = VectorI{int(floor((high_x - offset.x) / cell_size.width )) + 1,
                         int(floor((high_y - offset.y) / cell_size.height)) + 1};
    return RectangleI{start, convert_to<SizeI>(end - start)};
}

// ----------------------------------------------------------------------------

void GridIteration::for_each_sequence(SequenceInterface & seq_intf) {
    // It maybe possible to layer sweep container on top of this...
    //
    // I'd have to rewrite this though...
    for (auto & pair : m_view) {
        auto range = find_rectangle_range(pair.second, m_cell_size, m_offset);
        seq_intf.prestep(pair.second);
        for (int y = range.top ; y != bottom_of(range); ++y) {
        for (int x = range.left; x != right_of (range); ++x) {
            for (auto * other_ptr : find_cell(x, y)) {
                if (other_ptr == &pair.second) continue;
                // important note: it's totally okay to send dupelicate events!
                assert(other_ptr);
                seq_intf.step(pair.second, *other_ptr);
            }
        }}
        seq_intf.poststep(pair.second);
    }
}

const std::vector<FullEntry *> & GridIteration::find_cell(int x, int y) const {
    auto itr = m_grid.find(VectorI{x, y});
    if (itr != m_grid.end()) return itr->second;
    throw RtError("GridIteration::find_cell: grid was not properly updated. "
                  "(Entry exists in a cell not yet created.)");
}

// ----------------------------------------------------------------------------

void GridPhysicsHandlerImpl::set_offset(Vector offset) {
    if (!is_real(offset)) {
        throw InvArg("GridPhysicsHandlerImpl::set_offset: offset must be real "
                     "vector.");
    }
    m_offset = offset;
}

void GridPhysicsHandlerImpl::reset_grid_size
    (Real cell_width, Real cell_height)
{
    for (auto dim : { cell_width, cell_height }) {
        if (dim > 0 && is_real(dim)) continue;
        throw InvArg("GridPhysicsHandlerImpl::reset_grid_size: both width and "
                     "height must be positive real numbers");
    }
    m_cell_size = Size{cell_width, cell_height};
}

void GridPhysicsHandlerImpl::delete_empty_cells() {
    for (auto itr = m_pgrid.begin(); itr != m_pgrid.end(); ) {
        if (itr->second.empty()) itr = m_pgrid.erase(itr);
        else ++itr;
    }
}

/* private */ void GridPhysicsHandlerImpl::prepare_iteration
    (CollisionWorker & do_collision_work, EventHandler & event_handler)
{
    // there seems to be pretty significant overhead here
    // I have to old/new/delete them, otherwise impossible pairs will creep
    // to the narrow phase
    //
    // I think removing everything from the grid then adding them back may
    // in fact be both fastest (maybe) and simplest (high confidence)
    // implementation
    repopulate();

    GridIteration grid_iteration{entries_view(), m_pgrid, m_offset, m_cell_size};
    do_collision_work(event_handler, grid_iteration);

    // post finalization
    clear_all_cells();
}

/* private */ void GridPhysicsHandlerImpl::find_overlaps_
    (const Rectangle & rect, const OverlapInquiry & inq) const
{
    for (const auto & pair : entries_view()) {
        if (cul::find_rectangle_intersection(rect, pair.second.bounds).width > 0)
            inq(pair.second);
    }
}

/* private */ void GridPhysicsHandlerImpl::repopulate() {
    if (m_cell_size == Size{}) {
        throw RtError("GridPhysicsHandlerImpl::repopulate: cell size must be set.");
    }
    for (auto & pair : entries_view()) {
        update_broad_boundries(pair.second);
        auto range = find_rectangle_range(pair.second, m_cell_size, m_offset);
        for (int y = range.top ; y != bottom_of(range); ++y) {
        for (int x = range.left; x != right_of (range); ++x) {
            m_pgrid[VectorI{x, y}].push_back(&pair.second);
        }}
    }
}

/* private */ void GridPhysicsHandlerImpl::clear_all_cells() {
    for (auto & pair : m_pgrid) {
        pair.second.clear();
    }
}

} // end of tdp namespace
