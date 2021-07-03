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

#include <common/Vector2Util.hpp>

#include <cassert>

#ifdef MACRO_SWEEP_PRUNE_ALLOCATION_COUT_MSGS
#   include <iostream>
#endif

namespace tdp {

namespace detail {

// n = total number of elements
// m = largest blob of intersecting elements

// partitioned aabb map

template <typename ValueTypeT>
using PbmElement = std::tuple<Rectangle, ValueTypeT>;

template <typename ValueTypeT>
const Rectangle & get_rectangle(const PbmElement<ValueTypeT> & el)
    { return std::get<0>(el); }

template <typename ValueTypeT>
using PbmContainer = std::vector<PbmElement<ValueTypeT>>;

template <typename ValueTypeT>
using PbmContIter = typename PbmContainer<ValueTypeT>::iterator;

enum Orientation { k_horizontal, k_vertical };

template <typename ValueTypeT, Orientation kt_orientation>
class PartitionBoxMapPartition;

template <typename ValueTypeT>
using PbHorizontalPartMap = PartitionBoxMapPartition<ValueTypeT, k_horizontal>;

template <typename ValueTypeT>
using PbVerticalPartMap   = PartitionBoxMapPartition<ValueTypeT, k_vertical  >;

struct DivisionsExplorer {
    virtual ~DivisionsExplorer() {}
    virtual void operator () (const Rectangle & rect, int depth, bool is_deepest) const = 0;
};

template <typename ValueTypeT>
class PartitionedBoxMapBase {
public:
    virtual ~PartitionedBoxMapBase() {}
    // type is not visible in the "back"
    // I think I'm going to reintroduce templates...
    using Element           = PbmElement<ValueTypeT>;
    using UpdatingContainer = PbmContainer<ValueTypeT>;
    using UContIter         = PbmContIter<ValueTypeT>;

    static void print(const std::string & string) {
#   ifdef MACRO_SWEEP_PRUNE_ALLOCATION_COUT_MSGS
        std::cout << string << std::endl;
#   else
        (void)string;
        throw std::runtime_error("This function should not be called with this configuration.");
#   endif
    }

    struct ElementIterator {
        virtual ~ElementIterator() {}
        virtual void operator () (ValueTypeT) const = 0;
    };

    virtual void for_each
        (const Rectangle &, const ElementIterator &) const = 0;

    virtual void set_elements(UContIter beg, UContIter end) = 0;

    virtual void explore_divisions(const DivisionsExplorer &, int depth) const = 0;

    virtual Rectangle get_extreme_bounds() const = 0;

    virtual bool is_deepest_subcontainer() const { return false; }
};

template <typename ValueTypeT>
using PbmElementIterator = typename PartitionedBoxMapBase<ValueTypeT>::ElementIterator;

template <typename ValueTypeT>
class PartitionedBoxMapFlat final : public PartitionedBoxMapBase<ValueTypeT> {
public:
    using ElementIterator = PbmElementIterator<ValueTypeT>;
    using UContIter       = PbmContIter<ValueTypeT>;
    using BaseMap         = PartitionedBoxMapBase<ValueTypeT>;

    // O(n*m) across iterating n
    void for_each
        (const Rectangle & rect, const ElementIterator & itr) const final;

    // O(m)
    void set_elements(UContIter beg, UContIter end) final;

    // O(1)
    void explore_divisions(const DivisionsExplorer &, int) const final {}

    // O(m)
    Rectangle get_extreme_bounds() const final;

    bool is_deepest_subcontainer() const final { return true; }

private:
    PbmContainer<ValueTypeT> m_elements;
};

template <typename ValueTypeT>
struct PbMapFactory {
    using UContIter = PbmContIter<ValueTypeT>;

    virtual ~PbMapFactory() {}
    virtual PbVerticalPartMap<ValueTypeT> * ensure_vertical_partition() const = 0;
    virtual PbHorizontalPartMap<ValueTypeT> * ensure_horizontal_partition() const = 0;
    virtual PartitionedBoxMapFlat<ValueTypeT> * ensure_flat() const = 0;

    static PartitionedBoxMapBase<ValueTypeT> *
        choose(const PbMapFactory &, UContIter beg, UContIter end);
};

struct GetCountsRt {
    GetCountsRt() {}
    GetCountsRt(int l_, int s_, int h_):
        low_only(l_), shared(s_), high_only(h_) {}
    int low_only  = 0;
    int shared    = 0;
    int high_only = 0;
};

template <typename ValueTypeT, Orientation kt_orientation>
class PartitionBoxMapPartition final : public PartitionedBoxMapBase<ValueTypeT> {
public:
    using ElementIterator = PbmElementIterator<ValueTypeT>;
    using UContIter       = PbmContIter<ValueTypeT>;

    // note it is possible that a rectangle will go through both!
    // given that rect is the same location and size as any other element:
    // O(max(m*n, ...
    void for_each
        (const Rectangle & rect, const ElementIterator & itr) const final;

    void set_elements(UContIter beg, UContIter end) final;

    void explore_divisions(const DivisionsExplorer & explorer, int depth) const final;

    Rectangle get_extreme_bounds() const final;

    // ------------------------ implementation detail -------------------------
    // (revealed for testing)

    // O(n)
    // I'll need to test this in some fashion
    static Real get_division(UContIter beg, UContIter end);

    // this version recomputes the division
    // there are some cases where beg and end do not decide where the division
    // is
    static GetCountsRt get_counts(UContIter beg, UContIter end);

    // non-shared low, shared, non-shared high
    static GetCountsRt get_counts(Real div, UContIter beg, UContIter end);

    // partial sort in O(n)
    // this needs testing
    static void division_sort_low(Real div, UContIter beg, UContIter end);

    // partial sort in O(n)
    // this just needs a simple test (shouldn't be all that different from low
    // sort)
    static void division_sort_high(Real div, UContIter beg, UContIter end);

    static void check_order(Real div, UContIter beg, UContIter end);

    static bool on_low(const Rectangle & rect, Real div)
        { return get_position(rect) < div; }

    static bool on_high(const Rectangle & rect, Real div)
        { return get_position(rect) + get_length(rect) >= div; }

private:
    using MapBase = PartitionedBoxMapBase<ValueTypeT>;
    static Real get_length(const Rectangle & rect) {
        if constexpr (kt_orientation == k_vertical) return rect.height;
        else                                        return rect.width ;
    }

    static Real get_position(const Rectangle & rect) {
        if constexpr (kt_orientation == k_vertical) return rect.top ;
        else                                        return rect.left;
    }

    struct PartitionConts {
        PartitionedBoxMapFlat<ValueTypeT> flat;
        std::unique_ptr<PbHorizontalPartMap<ValueTypeT>> horz_partition;
        std::unique_ptr<PbVerticalPartMap<ValueTypeT>> vert_partition;
    };

    static void set_partition
        (PartitionConts & conts, MapBase *& ptr,
         UContIter beg, UContIter end, int parent_element_count);

    MapBase * m_low_partition  = nullptr;
    MapBase * m_high_partition = nullptr;
    PartitionConts m_low_part_conts, m_high_part_conts;
    Real m_division = 0.;
};

// maximum number of elements for a flat map
static constexpr const int k_pbm_partition_thershold = 3;//16;

// fraction describing how many overlapping elements causes the sp map
// to just use flat, rather than trying to fork
static constexpr const int k_pbm_overlap_thershold_num = 1;
static constexpr const int k_pbm_overlap_thershold_den = 2;

static constexpr const bool k_pbm_emit_allocation_messages =
#   ifdef MACRO_SWEEP_PRUNE_ALLOCATION_COUT_MSGS
    true
#   else
    false
#   endif
;

enum PbmStructureChoice { k_pbm_use_flat_only, k_pbm_use_partitioning };

static constexpr const auto k_pbm_default_partiting_choice = k_pbm_use_partitioning;

template <typename ValueTypeT,
          PbmStructureChoice kt_algo_choice = k_pbm_default_partiting_choice>
class PartitionBoxMap final {
public:
    using UpdatingContainer = PbmContainer<ValueTypeT>;
    using UContIter         = PbmContIter<ValueTypeT>;

    void set_elements(UContIter beg, UContIter end);

    template <typename Func>
    void for_each(const Rectangle & rect, Func && f) const;

    void explore_divisions(const DivisionsExplorer & explorer) const
        { m_root->explore_divisions(explorer, 0); }

    template <typename Func>
    void explore_divisions_f(Func && f) const;

private:
    using FlatMap = PartitionedBoxMapFlat<ValueTypeT>;
    using HorzMap = PbHorizontalPartMap<ValueTypeT>;
    using VertMap = PbVerticalPartMap<ValueTypeT>;
    using BaseMap = PartitionedBoxMapBase<ValueTypeT>;

    BaseMap * m_root = nullptr;
    FlatMap m_flat;
    HorzMap m_horz_partition;
    VertMap m_vert_partition;
};

// -------------------------- PartitionedBoxMapFlat ---------------------------

template <typename ValueTypeT>
void PartitionedBoxMapFlat<ValueTypeT>::for_each
    (const Rectangle &, const ElementIterator & itr) const
{
    for (auto & tuple : m_elements) {
        itr(std::get<1>(tuple));
    }
}

template <typename ValueTypeT>
void PartitionedBoxMapFlat<ValueTypeT>::set_elements(UContIter beg, UContIter end) {
    m_elements.clear();
    if (k_pbm_emit_allocation_messages && m_elements.capacity() < std::size_t(end - beg)) {
        BaseMap::print("[spmap] Allocating element buffer (" + std::to_string(end - beg) + ").");
    }
    m_elements.insert(m_elements.end(), beg, end);
}

template <typename ValueTypeT>
Rectangle PartitionedBoxMapFlat<ValueTypeT>::get_extreme_bounds() const {
    // an unoccupied partition should not be possible
    if (m_elements.empty()) return Rectangle();

    static constexpr const Real k_inf = std::numeric_limits<Real>::infinity();
    using std::min, std::max;

    Real low_x = k_inf, low_y = k_inf, high_x = -k_inf, high_y = -k_inf;
    for (const auto & el : m_elements) {
        const auto & rect = get_rectangle(el);
        low_x  = min(low_x , rect.left           );
        low_y  = min(low_y , rect.top            );
        high_x = max(high_x, cul::right_of (rect));
        high_y = max(high_y, cul::bottom_of(rect));
    }
    return Rectangle(low_x, low_y, high_x - low_x, high_y - low_y);
}

// ------------------------------- PbMapFactory -------------------------------

template <typename ValueTypeT>
/* static */ PartitionedBoxMapBase<ValueTypeT> * PbMapFactory<ValueTypeT>::choose
    (const PbMapFactory & factory, UContIter beg, UContIter end)
{
    if (end - beg <= k_pbm_partition_thershold) {
        return factory.ensure_flat();
    }

    auto hcounts = PbHorizontalPartMap<ValueTypeT>::get_counts(beg, end);
    auto vcounts = PbVerticalPartMap  <ValueTypeT>::get_counts(beg, end);
    auto fthresh = ((end - beg)*k_pbm_overlap_thershold_num)
                   / k_pbm_overlap_thershold_den;
    if (hcounts.shared < vcounts.shared) {
        if (hcounts.shared > fthresh || hcounts.shared == (end - beg))
            return factory.ensure_flat();
        return factory.ensure_horizontal_partition();
    } else {
        if (vcounts.shared > fthresh || vcounts.shared == (end - beg))
            return factory.ensure_flat();
        return factory.ensure_vertical_partition();
    }
}

// ------------------------- PartitionBoxMapPartition -------------------------

template <typename ValueTypeT, Orientation kt_orientation>
void PartitionBoxMapPartition<ValueTypeT, kt_orientation>::for_each
    (const Rectangle & rect, const ElementIterator & itr) const
{
    if (m_low_partition && on_low(rect, m_division)) {
        m_low_partition->for_each(rect, itr);
    }
    // assert(m_high_partition);
    if (on_high(rect, m_division)) {
        m_high_partition->for_each(rect, itr);
    }
}

template <typename ValueTypeT, Orientation kt_orientation>
void PartitionBoxMapPartition<ValueTypeT, kt_orientation>::set_elements
    (UContIter beg, UContIter end)
{
    // I think I'm missing something fundemental about sweep and prune here?
    // this blows up the whole thing to O(n*log(n)^2)

    // I don't need a complete sort here!
    // All I need is a "partial" sort
    m_division = get_division(beg, end);
    auto counts = get_counts(m_division, beg, end);
#   if 0 // should not be in published commit
    auto num = end - beg;
    std::array<Rectangle, 8> rect_copy;
    if (end - beg < int(rect_copy.size())) {
        auto witr = rect_copy.begin();
        for (auto itr = beg; itr != end; ++itr) {
            *witr++ = get_rectangle(*itr);
        }
    }
#   endif
    assert(beg + counts.low_only + counts.shared + counts.high_only == end);
    auto max_share = ((end - beg)*k_pbm_overlap_thershold_num)
                     / k_pbm_overlap_thershold_den;
    if (counts.shared > max_share) {
        throw std::runtime_error("number of shares too high for partitioned subcontainer.");
    }

    // should further test elements to make sure they are on the correct sides

    division_sort_low (m_division, beg, end);
    division_sort_high(m_division, beg, end);
    check_order(m_division, beg, end);
    set_partition(m_low_part_conts, m_low_partition, beg,
                  beg + counts.low_only + counts.shared, end - beg);

    // have to "fix" the low part again
    division_sort_low (m_division, beg, beg + counts.low_only + counts.shared);
    division_sort_high(m_division, beg, end);
    check_order(m_division, beg, end);

    set_partition(m_high_part_conts, m_high_partition,
                  beg + counts.low_only, end, end - beg);
}

template <typename ValueTypeT, Orientation kt_orientation>
void PartitionBoxMapPartition<ValueTypeT, kt_orientation>::explore_divisions
    (const DivisionsExplorer & explorer, int depth) const
{
    if (!m_low_partition) return;
    auto low  = get_extreme_bounds();
    auto high = low;
    // possibly compare function to pointer, not data member address
    // I'm not sure a second constexpr does anything or not
    if constexpr (kt_orientation == k_horizontal) {
        low.width = m_division - low.left;
    } else if constexpr (kt_orientation == k_vertical) {
        low.height = m_division - low.top;
    } else {
        throw std::runtime_error("bad branch");
    }

    if constexpr (kt_orientation == k_horizontal) {
        high.width = cul::right_of(high) - m_division;
        high.left  = m_division;
    } else if constexpr (kt_orientation == k_vertical) {
        high.height = cul::bottom_of(high) - m_division;
        high.top    = m_division;
    } else {
        throw std::runtime_error("bad branch");
    }
    explorer(low , depth, m_low_partition ->is_deepest_subcontainer());
    explorer(high, depth, m_high_partition->is_deepest_subcontainer());
    m_low_partition ->explore_divisions(explorer, depth + 1);
    m_high_partition->explore_divisions(explorer, depth + 1);
}

template <typename ValueTypeT, Orientation kt_orientation>
Rectangle PartitionBoxMapPartition<ValueTypeT, kt_orientation>::
    get_extreme_bounds() const
{
    if (!m_low_partition) return Rectangle();
    using std::max, cul::right_of, cul::bottom_of, std::min;
    auto low  = m_low_partition ->get_extreme_bounds();
    auto high = m_high_partition->get_extreme_bounds();
    Vector loc(min(low.left, high.left), min(low.top, high.top));
    Vector end(max(right_of(low), right_of(high)), max(bottom_of(low), bottom_of(high)));
    return Rectangle(loc, cul::convert_to<Size>(end - loc));
}

template <typename ValueTypeT, Orientation kt_orientation>
/* static */ Real PartitionBoxMapPartition<ValueTypeT, kt_orientation>::
    get_division
    (UContIter beg, UContIter end)
{
    using std::get;
    // beg to end need not be sorted here
    if (end == beg) return 0;
    // let's try a weight based on how far it is from the average point
    // so that farther objects more strongly drag the division toward it
    Real x_avg = 0;
    static auto get_x = [](const Rectangle & rect)
        { return get_position(rect) + get_length(rect)*0.5; };
    for (auto itr = beg; itr != end; ++itr) {
        x_avg += get_x(get_rectangle(*itr));
    }
    x_avg /= (end - beg);
    Real sum_diff = 0, sum_weights = 0;
    for (auto itr = beg; itr != end; ++itr) {
        const auto & rect = get_rectangle(*itr);
        auto pos  = get_position(rect);
        auto diff = cul::magnitude(x_avg - pos);

        sum_diff    += diff;
        sum_weights += diff*get_x(get_rectangle(*itr));
    }
    return sum_weights / sum_diff;
#   if 0
    Real sum_len = 0;
    for (auto itr = beg; itr != end; ++itr) {
        sum_len += get_length(std::get<0>(*itr));
    }

    Real sum_weights = 0;
    for (auto itr = beg; itr != end; ++itr) {
        const auto & rect = std::get<0>(*itr);
        auto len = get_length(rect);
        sum_len     += len;
        sum_weights += len*(get_position(rect) + len*0.5);
    }
    return sum_weights / sum_len;
#   endif
}

template <typename ValueTypeT, Orientation kt_orientation>
/* static */ GetCountsRt PartitionBoxMapPartition<ValueTypeT, kt_orientation>::
    get_counts(UContIter beg, UContIter end)
{ return get_counts(get_division(beg, end), beg, end); }

template <typename ValueTypeT, Orientation kt_orientation>
/* static */ GetCountsRt PartitionBoxMapPartition<ValueTypeT, kt_orientation>::
    get_counts
    (Real div, UContIter beg, UContIter end)
{
    // beg to end need not be sorted here either! c:
    int low = 0, shared = 0, high = 0;
    for (auto itr = beg; itr != end; ++itr) {
        const auto & rect = get_rectangle(*itr);
        if ( on_low(rect, div) && !on_high(rect, div)) ++low ;
        if (!on_low(rect, div) &&  on_high(rect, div)) ++high;
        if ( on_low(rect, div) &&  on_high(rect, div)) ++shared;
    }
    return GetCountsRt(low, shared, high);
}

template <typename ValueTypeT, Orientation kt_orientation>
/* static */ void PartitionBoxMapPartition<ValueTypeT, kt_orientation>::
    division_sort_low
    (Real div, UContIter beg, UContIter end)
{
    for (auto itr = beg; itr != end; ++itr) {
        if (!on_low(get_rectangle(*itr), div)) continue;
        std::swap(*itr, *beg++);
    }
}

template <typename ValueTypeT, Orientation kt_orientation>
/* static */ void PartitionBoxMapPartition<ValueTypeT, kt_orientation>::
    division_sort_high
    (Real div, UContIter beg, UContIter end)
{
    auto rbeg = std::make_reverse_iterator(end);
    auto rend = std::make_reverse_iterator(beg);
    for (auto itr = rbeg; itr != rend; ++itr) {
        if (!on_high(get_rectangle(*itr), div)) continue;
        std::swap(*itr, *rbeg++);
    }
}

template <typename ValueTypeT, Orientation kt_orientation>
/* static */ void PartitionBoxMapPartition<ValueTypeT, kt_orientation>::
    check_order
    (Real div, UContIter beg, UContIter end)
{
    using std::get;
    auto itr = beg;
    auto counts = get_counts(div, beg, end);
    for (; itr != beg + counts.low_only; ++itr) {
        // I need a test that makes this assertion fail
        assert(    on_low (get_rectangle(*itr), div)
               && !on_high(get_rectangle(*itr), div));
    }
    for (; itr != beg + counts.low_only + counts.shared; ++itr) {
        assert(   on_low (get_rectangle(*itr), div)
               && on_high(get_rectangle(*itr), div));
    }
    for (; itr != beg + counts.low_only + counts.shared + counts.high_only; ++itr) {
        assert(   !on_low (get_rectangle(*itr), div)
               &&  on_high(get_rectangle(*itr), div));
    }
}

template <typename ValueTypeT, Orientation kt_orientation>
/* private static */ void PartitionBoxMapPartition<ValueTypeT, kt_orientation>::
    set_partition
    (PartitionConts & conts, MapBase *& ptr,
     UContIter beg, UContIter end, int parent_element_count)
{
    using VertMap = PbVerticalPartMap<ValueTypeT>;
    using HorzMap = PbHorizontalPartMap<ValueTypeT>;
    using FlatMap = PartitionedBoxMapFlat<ValueTypeT>;
    class FactoryImpl final : public PbMapFactory<ValueTypeT> {
    public:
        FactoryImpl(PartitionConts & conts_): conts(conts_) {}
        VertMap * ensure_vertical_partition() const final {
            if (!conts.vert_partition) {
                conts.vert_partition = std::make_unique<VertMap>();
                if constexpr (k_pbm_emit_allocation_messages) {
                    MapBase::print("[spmap] Allocating new vertical partition.");
                }
            }
            return conts.vert_partition.get();
        }

        HorzMap * ensure_horizontal_partition() const final {
            if (!conts.horz_partition) {
                conts.horz_partition = std::make_unique<HorzMap>();
                if constexpr (k_pbm_emit_allocation_messages) {
                    MapBase::print("[spmap] Allocating new horizontal partition.");
                }
            }
            return conts.horz_partition.get();
        }

        FlatMap * ensure_flat() const final { return &conts.flat; }
        PartitionConts & conts;
    };

    if (beg == end) {
        throw std::invalid_argument("There may not be an empty partition.");
    }
    if (parent_element_count == end - beg) {
        ptr = &conts.flat;
    } else {
        FactoryImpl inst(conts);
        ptr = PbMapFactory<ValueTypeT>::choose(inst, beg, end);
    }
    ptr->set_elements(beg, end);
}

// ----------------------------- PartitionBoxMap ------------------------------

template <typename ValueTypeT, PbmStructureChoice kt_algo_choice>
void PartitionBoxMap<ValueTypeT, kt_algo_choice>::set_elements
    (UContIter beg, UContIter end)
{
    class FactoryImpl final : public PbMapFactory<ValueTypeT> {
    public:
        FactoryImpl(VertMap & vmap_, HorzMap & hmap_, FlatMap & fmap_):
            vmap(vmap_), hmap(hmap_), fmap(fmap_)
        {}

    private:
        VertMap * ensure_vertical_partition  () const final { return &vmap; }
        HorzMap * ensure_horizontal_partition() const final { return &hmap; }
        FlatMap * ensure_flat                () const final { return &fmap; }

        VertMap & vmap;
        HorzMap & hmap;
        FlatMap & fmap;
    };

    if constexpr (kt_algo_choice == k_pbm_use_flat_only) {
        m_root = &m_flat;
    } else if constexpr (kt_algo_choice == k_pbm_use_partitioning) {
        FactoryImpl inst(m_vert_partition, m_horz_partition, m_flat);
        m_root = PbMapFactory<ValueTypeT>::choose(inst, beg, end);
    }
    static constexpr const auto k_all_rectangles_must_be_real =
        "PartitionBoxMap::set_elements: All rectangles must have fields which "
        "are all real numbers.";
    static constexpr const auto k_size_must_be_non_negative =
        "PartitionBoxMap::set_elements: All rectangles must have non-negative "
        "sizes.";
    for (auto itr = beg; itr != end; ++itr) {
        using cul::is_real;
        const Rectangle & rect = get_rectangle(*itr);
        if (   !is_real(rect.left) || !is_real(rect.top) || !is_real(rect.width)
            || !is_real(rect.height))
        {
            throw std::runtime_error(k_all_rectangles_must_be_real);
        } else if (rect.width < 0 || rect.height < 0) {
            throw std::runtime_error(k_size_must_be_non_negative);
        }
    }
    m_root->set_elements(beg, end);
}

template <typename ValueTypeT, PbmStructureChoice kt_algo_choice>
template <typename Func>
void PartitionBoxMap<ValueTypeT, kt_algo_choice>::for_each
    (const Rectangle & rect, Func && f) const
{
    class IterImpl final : public PbmElementIterator<ValueTypeT> {
    public:
        IterImpl(Func && f_): f(std::move(f_)) {}
        void operator () (ValueTypeT v) const final { f(std::move(v)); }
        Func f;
    };
    IterImpl inst(std::move(f));
    m_root->for_each(rect, inst);
}

template <typename ValueTypeT, PbmStructureChoice kt_algo_choice>
template <typename Func>
void PartitionBoxMap<ValueTypeT, kt_algo_choice>::explore_divisions_f
    (Func && f) const
{
    struct Impl final : public DivisionsExplorer {
        Impl(Func && f_): f(f_) {}
        void operator () (const Rectangle & rect, int depth, bool is_deepest) const final
            { f(rect, depth, is_deepest); }
        Func f;
    };
    Impl inst(std::move(f));
    explore_divisions(inst);
}

} // end of detail namespace -> into ::tdp

} // end of tdp namespace
