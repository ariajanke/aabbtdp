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

#include <aabbtdp/physics.hpp>

#include "helpers.hpp"
#include "CollisionHandler.hpp"

namespace tdp {

using VectorI    = cul::Vector2  <int>;
using RectangleI = cul::Rectangle<int>;
using SizeI      = cul::Size2    <int>;

VectorI find_rectangle_start
    (Real low_x, Real low_y, const Size & cell_size, const Vector & offset);

inline VectorI find_rectangle_start
    (const FullEntry & entry, const Size & cell_size, const Vector & offset)
{ return find_rectangle_start(entry.low_x, entry.low_y, cell_size, offset); }

RectangleI find_rectangle_range
    (Real low_x, Real low_y, Real high_x, Real high_y,
     const Size & cell_size, const Vector & offset);

inline RectangleI find_rectangle_range
    (const FullEntry & entry, const Size & cell_size, const Vector & offset)
{
    return find_rectangle_range(entry.low_x, entry.low_y, entry.high_x, entry.high_y,
                                cell_size, offset);
}

struct VectorIHasher {
    std::size_t operator () (const VectorI & r) const {
        std::hash<int> int_hash{};
        return int_hash(r.x) ^ int_hash(r.y);
    }
};

using GridPhysicsMapElement = std::vector<FullEntry *>;
using GridPhysicsMap        = std::unordered_map<VectorI, GridPhysicsMapElement, VectorIHasher>;

// key into by entry pair, including one nullptr for wall hits
// extra data is: is_push, { is_trespass | is_solid }

class GridIteration final : public IterationBase {
public:
    GridIteration(EntryMapView view, const GridPhysicsMap & grid,
                  const Vector & offset, const Size & cell_size):
        m_view(view), m_grid(grid), m_offset(offset), m_cell_size(cell_size)
    {}

    void for_each_sequence(SequenceInterface &) final;

private:
    const std::vector<FullEntry *> & find_cell(int x, int y) const;

    EntryMapView m_view;
    const GridPhysicsMap & m_grid;
    const Vector & m_offset;
    const Size & m_cell_size;
};

class GridPhysicsHandlerImpl final :
    public GridPhysicsHandler, public CollisionHandler
{
public:
    void set_offset(Vector offset) final;

    void reset_grid_size(Real cell_width, Real cell_height) final;

    void delete_empty_cells() final;

    Vector offset() const { return m_offset; }

    Size cell_size() const { return m_cell_size; }

    template <typename Func>
    void count_each_cell(Func && f) {
        repopulate();
        for (auto & pair : m_pgrid)
            { f(pair.first, int(pair.second.size())); }
        clear_all_cells();
    }

private:
    void prepare_iteration(CollisionWorker &, EventHandler &) final;

    void find_overlaps_(const Rectangle &, const OverlapInquiry &) const final;

    void repopulate();

    void clear_all_cells();

    Size m_cell_size;
    Vector m_offset;
    GridPhysicsMap m_pgrid;
};

} // end of tdp namespace
