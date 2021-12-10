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

#include <common/TestSuite.hpp>
#include <common/Vector2Util.hpp>

#include <aabbtdp/defs.hpp>

#include "../src/SpatialMap.hpp"
#include "../src/sight-detail.hpp"

#include <cassert>

namespace {

// essentially the whole namespace
using cul::ts::TestSuite, cul::ts::test, cul::ts::Unit,
      tdp::sum_counts, tdp::only_on_high,
      tdp::only_on_low, tdp::SpatialMapElementGetters,
      tdp::SpatialMapFactory, tdp::PartitionedSpatialMap,
      tdp::get_counts, tdp::FlatSpatialMap,
      tdp::SpatialMapCounts, tdp::SpatialMap,
      tdp::pivot_sort_around, tdp::pivot_sort, tdp::Real,
      tdp::Rectangle, tdp::Vector,// tdp::detail::AngleGetters,
      tdp::PolarVector, tdp::ImageEntry;

struct UniDimRecord {
    UniDimRecord() {}
    UniDimRecord(Real pos_, Real len_, std::string name_ = ""):
        position(pos_),
        length  (len_),
        name    (name_)
    {}

    Real position = 0, length = 0;
    std::string name;
};

struct UniDimRecordInterface final :
    public SpatialMapElementGetters<UniDimRecord>
{
    Real get_low(const UniDimRecord & rec) const final { return rec.position; }
    Real get_high(const UniDimRecord & rec) const final { return rec.position + rec.length; }
};

constexpr const int k_test_factory_max_depth = 100;
constexpr const auto k_max_depth_exceeded_msg =
    "Maximum recursion depth for factory functions reached... this shouldn't "
    "happen in testing code.";
using RtError = std::runtime_error;

struct UniDimMapFactory final :
    public SpatialMapFactory<UniDimRecord>
{
    using PartMap = PartitionedSpatialMap<UniDimRecord, UniDimRecordInterface, UniDimMapFactory>;
    static constexpr const int k_fork_thershold = 4;
    // if shares greater than 1/2 or count < fork_thershold
    MapBase & choose_map_for(SetElIterator beg, SetElIterator end, int depth) final {
        if (depth > k_test_factory_max_depth) throw RtError(k_max_depth_exceeded_msg);
        if (end - beg < k_fork_thershold) return m_flat;
        Real avg_pt = 0;
        for (auto itr = beg; itr != end; ++itr) {
            avg_pt += (**itr).position + (**itr).length / 2;
        }
        avg_pt /= Real(end - beg);
        auto counts = get_counts<UniDimRecord, UniDimRecordInterface>(avg_pt, beg, end);
        if (counts.shared > (counts.on_high + counts.on_low + counts.shared) / 2) {
            return m_flat;
        }
        if (!m_part_map) {
            m_part_map = std::make_unique<PartMap>();
        }
        m_part_map->set_division(avg_pt);
        return *m_part_map;
    }

    FlatSpatialMap<UniDimRecord> m_flat;
    std::unique_ptr<PartMap> m_part_map;
};

struct BiDimRecord {
    BiDimRecord() {}
    BiDimRecord(Real left_, Real top_, Real wid_, Real hei_):
        bounds(left_, top_, wid_, hei_)
    {}
    Rectangle bounds;
    std::string name;
};

struct HorzRecordInterface final : public SpatialMapElementGetters<BiDimRecord> {
    Real get_low(const BiDimRecord & rec) const final
        { return rec.bounds.left; }

    Real get_high(const BiDimRecord & rec) const final
        { return cul::right_of(rec.bounds); }
};

struct VertRecordInterface final : public SpatialMapElementGetters<BiDimRecord> {
    Real get_low(const BiDimRecord & rec) const final
        { return rec.bounds.top; }

    Real get_high(const BiDimRecord & rec) const final
        { return cul::bottom_of(rec.bounds); }
};

struct BiDimMapFactory final : public SpatialMapFactory<BiDimRecord> {
    // these are essentially a forward declarations
    using HorzPartMap = PartitionedSpatialMap<BiDimRecord, HorzRecordInterface, BiDimMapFactory>;
    using VertPartMap = PartitionedSpatialMap<BiDimRecord, VertRecordInterface, BiDimMapFactory>;

    // fixed for testing purposes
    static constexpr const Real k_division = 8;

    // if shares greater than 1/2 or count < fork_thershold
    MapBase & choose_map_for(SetElIterator beg, SetElIterator end, int depth) final {
#       if 0
        std::cout << (end - beg) << " items in depth " << depth << " level(s)" << std::endl;
#       endif
        switch (depth) {
        case 0:
            if (!m_horz_map_ptr) {
                m_horz_map_ptr = std::make_unique<HorzPartMap>();
            }
            m_horz_map_ptr->set_division(k_division);
            return *m_horz_map_ptr;
        case 1: // at this depth, should be divided into quadrants
            if (!m_vert_map_ptr) {
                m_vert_map_ptr = std::make_unique<VertPartMap>();
            }
            m_vert_map_ptr->set_division(k_division);
            return *m_vert_map_ptr;
        case 2:
            return m_flat;
        default: throw RtError(k_max_depth_exceeded_msg);
        }
    }

    FlatSpatialMap<BiDimRecord> m_flat;
    std::unique_ptr<HorzPartMap> m_horz_map_ptr;
    std::unique_ptr<VertPartMap> m_vert_map_ptr;
};

// --- other helpers for testing ---

template <typename T>
std::vector<T *> convert_to_pointer_vector(std::vector<T> & vec) {
    std::vector<T *> rv;
    rv.reserve(vec.size());
    for (auto & r : vec) rv.push_back(&r);
    return rv;
}

using UniDimRecPtrConstIter = std::vector<UniDimRecord *>::const_iterator;

bool any_has_position(Real pos, UniDimRecPtrConstIter beg, UniDimRecPtrConstIter end) {
    for (auto itr = beg; itr != end; ++itr) {
        if (std::equal_to<Real>{}((**itr).position, pos)) {
            return true;
        }
    }
    return false;
}

} // end of <anonymous> namespace

#define mark MACRO_MARK_POSITION_OF_CUL_TEST_SUITE

void do_spatial_map_unit_tests(TestSuite & suite) {
    suite.start_series("only_on_* functions");
    mark(suite).test([] {
        return test(only_on_low(UniDimRecordInterface{}, UniDimRecord{ -10, 5 }, 0));
    });
    mark(suite).test([] {
        return test(only_on_high(UniDimRecordInterface{}, UniDimRecord{ 1, 0.5 }, 0));
    });
    mark(suite).test([] {
        UniDimRecord rec{ -0.5, 1 };
        return test(   !only_on_low (UniDimRecordInterface{}, rec, 0)
                    && !only_on_high(UniDimRecordInterface{}, rec, 0));
    });
    set_context(suite, [](TestSuite & suite, Unit & unit) {
        constexpr const Real k_pi = cul::k_pi_for_type<Real>;
        struct PiWrap final : public SpatialMapElementGetters<UniDimRecord> {
            Real get_low (const UniDimRecord & rec) const final
                { return rec.position; }

            Real get_high(const UniDimRecord & rec) const final
                { return rec.position + rec.length; }

            Real domain_max() const final { return  k_pi; }
            Real domain_min() const final { return -k_pi; }
        };
        unit.start(mark(suite), [] {
            // test wrap around
            return test(only_on_low(PiWrap{}, UniDimRecord { -1.1, 0.4 }, 0));
        });
        unit.start(mark(suite), [] {
            return test(!only_on_low(PiWrap{}, UniDimRecord { -3.3, 0.5 }, 0));
        });
        unit.start(mark(suite), [] {
            return test(only_on_high(PiWrap{}, UniDimRecord { 2.4, 0.4 }, 0));
        });
        unit.start(mark(suite), [] {
            return test(!only_on_high(PiWrap{}, UniDimRecord { 2.8, 0.4 }, 0));
        });
        unit.start(mark(suite), [] {
            UniDimRecord rec{ -0.2, 0.4 };
            return test(!only_on_low(PiWrap{}, rec, 0) && !only_on_high(PiWrap{}, rec, 0));
        });
    });
#   if 0
    // observed failure 21-8-3 1111
    mark(suite).test([] {
        // this test indicates a failure in image production
        ImageEntry image;
        image.anchor_low  = Vector{-(142 + 2. / 3.), -(101 + 2. / 3.)};
        image.anchor_high = Vector{-(132 + 2. / 3.), -(111 + 2. / 3.)};
        assert(PolarVector{image.anchor_low}.theta < PolarVector{image.anchor_high}.theta);
        Real div = -2.507050;
        return test( !(   only_on_low (AngleGetters{}, image, div)
                       && only_on_high(AngleGetters{}, image, div)) );
    });
    // observed failure 21-8-4 1042
    mark(suite).test([] {
        ImageEntry image;
        // this crosses the domain line, so it must be shared
        image.anchor_low = Vector{-129.858, 5.09307};
        image.anchor_high = Vector{-129.858, -4.90693};
        PolarVector pal{image.anchor_low};
        PolarVector pah{image.anchor_high};
        //assert(PolarVector{image.anchor_low}.theta < PolarVector{image.anchor_high}.theta);
        Real div = -1.46847;
        return test(   !only_on_low (AngleGetters{}, image, div)
                    && !only_on_high(AngleGetters{}, image, div) );
    });
#   endif
    using TestUniMap = SpatialMap<UniDimRecord, UniDimMapFactory>;
    suite.start_series("spatial partition map");
    set_context(suite, [](TestSuite & suite, Unit & unit) {
        TestUniMap map;
        unit.start(mark(suite), [&map] {
            std::vector<UniDimRecord> col = {
                UniDimRecord { 0  , 1 },
                UniDimRecord { 0.5, 1 },
                UniDimRecord { 1  , 1 }
            };
            map.set_elements(col.begin(), col.end());
            // the partition map does not handle intersection logic
            // it only collects possible intersections
            return test(map.collect_candidates(UniDimRecord{ 0, 0.1 }).size() == 3);
        });

    });
    // lets test partial sort
    mark(suite).test([] {
        using namespace cul::exceptions_abbr;
        std::vector<int> col = { 9, 8, 3, 2, 1, 4, 10, 4 };
        auto on_low = [](int i) { return i <= 4; };
        pivot_sort(col.begin(), col.end(), on_low);
        auto itr = col.begin();
        int low_count = 0;
        for (; itr != col.end(); ++itr) {
            if (!on_low(*itr)) break;
            ++low_count;
        }
        int high_count = 0;
        for (; itr != col.end(); ++itr) {
            if (on_low(*itr)) throw RtError("partial sort failed");
            ++high_count;
        }
        return test(low_count == 5 && high_count == 3);
    });
    // two items

    //
    mark(suite).test([] {
        std::vector<UniDimRecord> col = {
            UniDimRecord { 0  , 1 },
            UniDimRecord { 1.5, 1 },
            UniDimRecord { 3.0, 1 }
        };
        return test(   !only_on_low (UniDimRecordInterface{}, col[1], 2)
                    && !only_on_high(UniDimRecordInterface{}, col[1], 2));
    });

    mark(suite).test([] {
        std::vector<UniDimRecord> col = {
            UniDimRecord { 0  , 1 },
            UniDimRecord { 1.5, 1 },
            UniDimRecord { 3.0, 1 }
        };
        auto ptrcol = convert_to_pointer_vector(col);
        auto gv = pivot_sort_around<UniDimRecord, UniDimRecordInterface>(ptrcol.begin(), ptrcol.end(), 2);
        return test(   gv.high_beg == ptrcol.begin() + 1
                    && gv.low_end  == ptrcol.begin() + 2);
    });
    // do a partial sort that actually changes the order of the elements
    // both one side (moving low only)
    mark(suite).test([] {
        std::vector col = {
            UniDimRecord { 0.5, 1 },
            UniDimRecord { 1.5, 1 },
            UniDimRecord { 2.5, 1 },
            UniDimRecord { 3.0, 1 },
            UniDimRecord { 0  , 1 } // <- this needs to be moved
        };
        auto ptrcol = convert_to_pointer_vector(col);
        auto gv = pivot_sort_around<UniDimRecord, UniDimRecordInterface>(ptrcol.begin(), ptrcol.end(), 2);
        return test(   gv.low_end - ptrcol.begin() == 3
                    && any_has_position(0, ptrcol.begin(), gv.low_end));
    });
    // and two sided (low and high)
    mark(suite).test([] {
        std::vector col = {
            UniDimRecord { 3.0, 1 }, // <- this needs to be moved
            UniDimRecord { 0.5, 1 },
            UniDimRecord { 1.5, 1 },
            UniDimRecord { 2.5, 1 },
            UniDimRecord { 0  , 1 } // <- as does this
        };
        auto ptrcol = convert_to_pointer_vector(col);
        auto gv = pivot_sort_around<UniDimRecord, UniDimRecordInterface>(ptrcol.begin(), ptrcol.end(), 2);
        return test(   any_has_position(3, gv.high_beg   , ptrcol.end())
                    && any_has_position(0, ptrcol.begin(), gv.low_end  ));

    });
    // test low only candidates
    mark(suite).test([] {
        TestUniMap map;
        std::vector<UniDimRecord> col = {
            UniDimRecord { 0  , 1 },
            UniDimRecord { 0.5, 1 },
            UniDimRecord { 2.5, 1 },
            UniDimRecord { 3.0, 1 }
        };
        map.set_elements(col.begin(), col.end());
        return test(map.collect_candidates(UniDimRecord{0.75, 0.5}).size() == 2);
    });
    // test shared candidate
    set_context(suite, [](TestSuite & suite, Unit & unit) {
        // avg should be 2
        std::vector<UniDimRecord> col = {
            UniDimRecord { 0  , 1 },
            UniDimRecord { 0.5, 1 },
            UniDimRecord { 1.5, 1 },
            UniDimRecord { 2.5, 1 },
            UniDimRecord { 3.0, 1 }
        };
        TestUniMap map;
        map.set_elements(col.begin(), col.end());
        unit.start(mark(suite), [&] {
            return test(map.collect_candidates(UniDimRecord{ 1.5, 0.4 }).size() == 3);
        });
        unit.start(mark(suite), [&] {
            return test(map.collect_candidates(UniDimRecord{ 2.2, 0.4 }).size() == 3);
        });
        unit.start(mark(suite), [&] {
            // we get *all* elements are candidate interactions
            return test(map.collect_candidates(UniDimRecord{ 1.8, 0.4 }).size() == 5);
        });
    });
    using TestBiMap = SpatialMap<BiDimRecord, BiDimMapFactory>;
    set_context(suite, [](TestSuite & suite, Unit & unit) {
        std::vector col = {
            BiDimRecord{  2,  2, 2, 2 },
            BiDimRecord{  4,  4, 2, 2 },
            BiDimRecord{ 10, 10, 2, 2 },
            BiDimRecord{ 12, 12, 2, 2 },
            BiDimRecord{  4,  7, 2, 2 },
            BiDimRecord{  2, 12, 3, 3 },
            BiDimRecord{  7, 12, 2, 3 }
        };
        for (auto & entry : col) {
            bool on_right  = entry.bounds.left > BiDimMapFactory::k_division;
            bool on_bottom = entry.bounds.top  > BiDimMapFactory::k_division;
            entry.name += on_bottom ? "b" : "t";
            entry.name += on_right  ? "r" : "l";
        }
#       if 0
        (() => {
            let col = [ [  2,  2, 2, 2 ],
                        [  4,  4, 2, 2 ],
                        [ 10, 10, 2, 2 ],
                        [ 12, 12, 2, 2 ],
                        [  4,  7, 2, 2 ],
                        [  2, 12, 3, 3 ],
                        [  7, 12, 2, 3 ] ];
            const context = $('#canvas').getContext('2d');
            const draw_divs = () => {
                    context.strokeStyle = '#F99';
                    context.beginPath();
                    context.moveTo(0*10, 8*10);
                    context.lineTo(16*10, 8*10);
                    context.closePath();
                    context.stroke();
                    context.beginPath();
                    context.moveTo(8*10, 0*10);
                    context.lineTo(8*10, 16*10);
                    context.closePath();
                    context.stroke();
            };
            const draw_rect = (e, outline_style, fill_style) => {
                outline_style = outline_style || '#F0F';
                fill_style    = fill_style    || '#000';
                context.fillStyle = outline_style;
                context.fillRect(e[0]*10 - 1, e[1]*10 - 1, e[2]*10 + 2, e[3]*10 + 2);
                context.fillStyle = fill_style;
                context.fillRect(e[0]*10, e[1]*10, e[2]*10, e[3]*10);
            };
            context.fillRect(0, 0, 400, 400);
            context.fillStyle = '#FFF';
            $('#canvas').getContext('2d').fillRect(0, 0, 400, 400);
            col.forEach(e => draw_rect(e));
            draw_rect([12, 13, 2, 2], '#F0F', '#FF0');
            draw_divs();
        })();
#       endif
        TestBiMap map;
        map.set_elements(col.begin(), col.end());
        unit.start(mark(suite), [&] {
            auto cands = map.collect_candidates(BiDimRecord{12, 13, 2, 2});
            return test(cands.size() == 3);
        });
        unit.start(mark(suite), [&] {
            auto cands = map.collect_candidates(BiDimRecord{4, 13, 2, 2});
            return test(cands.size() == 3);
        });
    });
}
