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

namespace {

using std::get, tdp::detail::FullEntry,
      tdp::detail::Tuple, cul::right_of, cul::bottom_of, std::floor;

} // end of <anonymous> namespace

namespace tdp {

namespace detail {

RectangleI find_rectangle_range
    (Real low_x, Real low_y, Real high_x, Real high_y,
     const Size & cell_size, const Vector & offset)
{
    if (cell_size.width == 0 || cell_size.height == 0) return RectangleI{};

    low_x  -= offset.x;
    high_x -= offset.x;
    low_y  -= offset.y;
    high_y -= offset.y;

    int start_x = int(floor(low_x / cell_size.width ));
    int start_y = int(floor(low_y / cell_size.height));

    return RectangleI{
        start_x, start_y,
        int(floor(high_x / cell_size.width )) + 1 - start_x,
        int(floor(high_y / cell_size.height)) + 1 - start_y
    };
}

void GridPhysicsHandlerImpl::update_entry(const Entry & entry) {
    m_info.update_entry(entry);
}

void GridPhysicsHandlerImpl::run(EventHandler &) {
    // there seems to be pretty significant overhead here
    // I have to old/new/delete them, otherwise impossible pairs will creep
    // to the narrow phase
    //
    // I think removing everything from the grid then adding them back may
    // in fact be both fastest (maybe) and simplest (high confidence)
    // implementation
    repopulate();

    // with grid there's a non-zero chance for dupelicates
    // that is two seperate cells may share the same interaction pair

    // post finalization
    for (auto & pair : m_pgrid) {
        pair.second.clear();
    }
    repopulate();
}

void GridPhysicsHandlerImpl::set_offset(Vector offset) {
    m_offset = offset;
}

void GridPhysicsHandlerImpl::reset_grid_size
    (Real cell_width, Real cell_height)
{
    m_cell_size = Size{cell_width, cell_height};
}

void GridPhysicsHandlerImpl::delete_empty_cells() {
    for (auto itr = m_pgrid.begin(); itr != m_pgrid.end(); ) {
        if (itr->second.empty()) itr = m_pgrid.erase(itr);
        else ++itr;
    }
}

/* private */ void GridPhysicsHandlerImpl::find_overlaps_
    (const Rectangle &, const OverlapInquiry &) const
{

}

/* private */ void GridPhysicsHandlerImpl::repopulate() {
    for (auto & pair : m_info.entries_view()) {
        auto range = find_rectangle_range(pair.second, m_cell_size, m_offset);
        for (int y = range.top ; y != bottom_of(range); ++y) {
        for (int x = range.left; x != right_of (range); ++x) {
            m_pgrid[VectorI{x, y}].push_back(&pair.second);
        }}
    }
}

} // end of detail namespace -> into ::tdp

} // end of tdp namespace
