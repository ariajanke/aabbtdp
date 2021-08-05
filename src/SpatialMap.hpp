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

#include <common/Util.hpp>

namespace tdp {

namespace detail {

template <typename ValueTypeT>
using SpElementContainer = std::vector<ValueTypeT *>;

template <typename ValueType>
using SpIterator = typename std::vector<ValueType *>::iterator;

template <typename ValueType>
using SpConstIterator = typename std::vector<ValueType *>::const_iterator;

template <typename ValueTypeT>
struct SpatialMapDivisionsExplorer {
    virtual ~SpatialMapDivisionsExplorer() {}
    virtual void operator () (const ValueTypeT &, int depth) const = 0;
};

template <typename ValueTypeT>
class SpatialMapBase {
public:
    virtual ~SpatialMapBase() {}
    // type is not visible in the "back"
    // I think I'm going to reintroduce templates...
    using Element           = ValueTypeT;
    using SetElIterator     = SpIterator<ValueTypeT>;
    using ElementContainer  = SpElementContainer<ValueTypeT>;
    using DivisionsExplorer = SpatialMapDivisionsExplorer<ValueTypeT>;

    static void print(const std::string & string) {
#   ifdef MACRO_SWEEP_PRUNE_ALLOCATION_COUT_MSGS
        std::cout << string << std::endl;
#   else
        (void)string;
        throw std::runtime_error("This function should not be called with this configuration.");
#   endif
    }

    // no guarentee to avoid dupelicates
    virtual void collect_candidates(const Element &, ElementContainer &) const = 0;

    // making this even safer: make a version that does not need to be recursive
    // (though having a limiting variable/constant for depth maybe fine for now)
    virtual void set_elements(SetElIterator beg, SetElIterator end, int depth) = 0;

    virtual void explore_partitions(const DivisionsExplorer &, const Element &, int depth) const = 0;
};

template <typename ValueTypeT>
class FlatSpatialMap final : public SpatialMapBase<ValueTypeT> {
public:
    using Element           = ValueTypeT;
    using SetElIterator     = SpIterator<ValueTypeT>;
    using ElementContainer  = SpElementContainer<ValueTypeT>;
    using DivisionsExplorer = SpatialMapDivisionsExplorer<ValueTypeT>;

    void set_elements(SetElIterator beg, SetElIterator end, int) final;

    void collect_candidates(const Element &, ElementContainer &) const final;

    void explore_partitions(const DivisionsExplorer &, const Element &, int depth) const final;

private:
    ElementContainer m_container;
};

template <typename ValueTypeT>
class SpatialMapElementGetters {
public:
    using MapBase = SpatialMapBase<ValueTypeT>;
    using Element = ValueTypeT;

    virtual ~SpatialMapElementGetters() {}
    virtual Real get_low (const Element &) const = 0;
    virtual Real get_high(const Element &) const = 0;

    virtual Real domain_max() const { return  k_inf; }
    virtual Real domain_min() const { return -k_inf; }

    struct ElementCouple { Element low_side, high_side; };

    // default implementation throws...
    // (optionally overriden, but fails if the feature is ever used)
    virtual ElementCouple divide_for_exploration(const Element &, Real division) const;

private:
    static constexpr const Real k_inf = std::numeric_limits<Real>::infinity();
};

// rules for map factory:
// may include a flat map
// may NOT include a partitioned map (unless a pointer)
template <typename ValueTypeT>
struct SpatialMapFactory {
    using MapBase       = SpatialMapBase<ValueTypeT>;
    using SetElIterator = SpIterator<ValueTypeT>;
    // should not need to be copyable?

    virtual ~SpatialMapFactory() {}

    // implies that this is stateful
    virtual MapBase & choose_map_for(SetElIterator beg, SetElIterator end, int depth) = 0;
};

template <typename ValueTypeT, typename ObjIntf, typename MapFactoryT>
class PartitionedSpatialMap final : public SpatialMapBase<ValueTypeT> {
public:
    static_assert(std::is_base_of_v<SpatialMapElementGetters<ValueTypeT>, ObjIntf>,
        "Object interface must implement SpatialMapElementGetters.");

    static_assert(std::is_base_of_v<SpatialMapFactory<ValueTypeT>, MapFactoryT>,
        "Map factory must implement SpatialMapFactory.");

    using Element           = ValueTypeT;
    using SetElIterator     = SpIterator<ValueTypeT>;
    using ElementContainer  = SpElementContainer<ValueTypeT>;
    using DivisionsExplorer = SpatialMapDivisionsExplorer<ValueTypeT>;

    // at this point we've already figured out that we want this partition map
    void set_elements(SetElIterator beg, SetElIterator end, int depth) final;

    // so we should know the division we want too?
    void set_division(Real);

    void collect_candidates(const Element &, ElementContainer &) const final;

    void explore_partitions(const DivisionsExplorer &, const Element &, int depth) const final;

private:
    bool on_low(const Element &) const;

    bool on_high(const Element &) const;

    static constexpr const Real k_inf = std::numeric_limits<Real>::infinity();
    using MapBase = SpatialMapBase<ValueTypeT>;

    MapFactoryT m_low_factory, m_high_factory;

    MapBase * m_low_partition  = nullptr;
    MapBase * m_high_partition = nullptr;

    Real m_division = k_inf;
};

// perhaps "SpatialMap" would be a better name?
template <typename ValueTypeT, typename MapFactoryT>
class SpatialMap final {
public:
    static_assert(std::is_base_of_v<SpatialMapFactory<ValueTypeT>, MapFactoryT>,
        "Map factory must implement SpatialMapFactory.");

    using Element           = ValueTypeT;
    using ElementContainer  = SpElementContainer<ValueTypeT>;
    using DivisionsExplorer = SpatialMapDivisionsExplorer<ValueTypeT>;

    template <typename IterType>
    void set_elements(IterType beg, IterType end);

    template <typename IterType, typename Func>
    void set_elements(IterType beg, IterType end, Func && convert_iterator_to_pointer);

    ElementContainer collect_candidates(const Element &, ElementContainer && = ElementContainer()) const;

    template <typename Func>
    void explore_partitions(const Element &, Func &&) const;

private:
    void explore_partitions_(const DivisionsExplorer &, const Element &) const;

    static SpatialMapBase<Element> & default_base();

    using MapBase = SpatialMapBase<Element>;
    std::vector<Element *> m_pointers;
    MapFactoryT m_top_level_factory;
    MapBase * m_base = &default_base();
};

template <typename ObjIntf, typename ValueTypeT>
constexpr const bool kt_is_sp_element_getters =
    std::is_base_of_v<SpatialMapElementGetters<ValueTypeT>, ObjIntf>;

template <typename ObjIntf, typename ValueTypeT, typename T>
using EnableElementGettersType = std::enable_if_t<kt_is_sp_element_getters<ObjIntf, ValueTypeT>, T>;

// This test if you're *strictly* on low
// if the element is neither, then it's shared
template <typename ObjIntf, typename ValueTypeT>
EnableElementGettersType<ObjIntf, ValueTypeT, bool>
    only_on_low(ObjIntf, const ValueTypeT & obj, Real division)
{
    static_assert(kt_is_sp_element_getters<ObjIntf, ValueTypeT>, "");
    static constexpr const Real k_inf = std::numeric_limits<Real>::infinity();

    bool behind_div = ObjIntf{}.get_high(obj) < division;
    if (!behind_div) return false;

    Real domain_min = ObjIntf{}.domain_min();
    if (-k_inf == domain_min) return true;
    Real low = ObjIntf{}.get_low(obj);
    return low >= domain_min && low < division;
}

// This test if you're *strictly* on high
// if the element is neither, then it's shared
template <typename ObjIntf, typename ValueTypeT>
EnableElementGettersType<ObjIntf, ValueTypeT, bool>
    only_on_high(ObjIntf, const ValueTypeT & obj, Real division)
{
    static_assert(kt_is_sp_element_getters<ObjIntf, ValueTypeT>, "");
    static constexpr const Real k_inf = std::numeric_limits<Real>::infinity();

    bool ahead_of_div = ObjIntf{}.get_low(obj) >= division;
    if (!ahead_of_div) return false;

    Real domain_max = ObjIntf{}.domain_max();
    if (k_inf == domain_max) return true;
    Real high = ObjIntf{}.get_high(obj);
    return high <= domain_max && high > division;
}

template <typename IterType, typename OnLowPred>
void pivot_sort(IterType beg, IterType end, OnLowPred && on_low);

template <typename ValueTypeT>
struct PivotSortAroundRt {
    SpIterator<ValueTypeT> high_beg;
    SpIterator<ValueTypeT> low_end ;
};

// O(n)
// returns high_beg, low_end
// note: high_beg <= low_end
// partially_sort_around will throw if m_division is not a real number
template <typename ValueTypeT, typename ObjIntf>
PivotSortAroundRt<ValueTypeT> pivot_sort_around
    (SpIterator<ValueTypeT> beg, SpIterator<ValueTypeT> end, Real division);

struct SpatialMapCounts {
    int on_low = 0, on_high = 0, shared = 0;
};

template <typename ValueTypeT, typename ObjIntf>
SpatialMapCounts get_counts
    (Real division, SpConstIterator<ValueTypeT> beg, SpConstIterator<ValueTypeT> end);

int sum_counts(const SpatialMapCounts &);

bool prefer_lhs_counts(const SpatialMapCounts & lhs, const SpatialMapCounts & rhs);

// ----------------------------------------------------------------------------

template <typename ValueTypeT>
void FlatSpatialMap<ValueTypeT>::set_elements
    (SetElIterator beg, SetElIterator end, int)
{
    m_container.clear();
    m_container.insert(m_container.begin(), beg, end);
}

template <typename ValueTypeT>
void FlatSpatialMap<ValueTypeT>::collect_candidates
    (const Element &, ElementContainer & outcont) const
{
    outcont.insert(outcont.begin(), m_container.begin(), m_container.end());
}

template <typename ValueTypeT>
void FlatSpatialMap<ValueTypeT>::explore_partitions
    (const DivisionsExplorer & explr, const Element & el, int depth) const
{ explr(el, depth); }

// ----------------------------------------------------------------------------

template <typename ValueTypeT>
typename SpatialMapElementGetters<ValueTypeT>::ElementCouple
    SpatialMapElementGetters<ValueTypeT>::divide_for_exploration
    (const Element &, Real) const
{
    using namespace cul::exceptions_abbr;
    throw RtError("SpatialMapElementGetters::divide_for_exploration: "
                  "implementation must be defined in the derived class if "
                  "calling this method is ever desired.");
}

// ----------------------------------------------------------------------------

template <typename ValueTypeT, typename ObjIntf, typename MapFactoryT>
void PartitionedSpatialMap<ValueTypeT, ObjIntf, MapFactoryT>::
    set_elements(SetElIterator beg, SetElIterator end, int depth)
{
    // will need to partial sort here...
    // partially_sort_around will throw if m_division is not a real number
    auto gv = pivot_sort_around<Element, ObjIntf>(beg, end, m_division);
    //auto c = end - beg;

    m_low_partition  = &m_low_factory .choose_map_for(beg        , gv.low_end, depth + 1);
    m_high_partition = &m_high_factory.choose_map_for(gv.high_beg, end       , depth + 1);

    m_low_partition ->set_elements(beg        , gv.low_end, depth + 1);
    m_high_partition->set_elements(gv.high_beg, end       , depth + 1);
}

template <typename ValueTypeT, typename ObjIntf, typename MapFactoryT>
void PartitionedSpatialMap<ValueTypeT, ObjIntf, MapFactoryT>::set_division
    (Real r)
{
    using namespace cul::exceptions_abbr;
    if (!cul::is_real(r)) {
        throw InvArg("PartitionedSpatialMap::set_division: division must be "
                     "a real number.");
    }
    m_division = r;
}

template <typename ValueTypeT, typename ObjIntf, typename MapFactoryT>
void PartitionedSpatialMap<ValueTypeT, ObjIntf, MapFactoryT>
    ::collect_candidates(const Element & element, ElementContainer & outcont) const
{
    if (on_low(element) && on_high(element)) {
        using namespace cul::exceptions_abbr;
        throw RtError("PartitionedSpatialMap::collect_candidates: an element "
                      "cannot be both \"exclusively\" be on the low side and "
                      "high side.");
    } else if (on_low(element)) {
        m_low_partition->collect_candidates(element, outcont);
    } else if (on_high(element)) {
        m_high_partition->collect_candidates(element, outcont);
    } else {
        m_low_partition ->collect_candidates(element, outcont);
        m_high_partition->collect_candidates(element, outcont);
    }
}

template <typename ValueTypeT, typename ObjIntf, typename MapFactoryT>
void PartitionedSpatialMap<ValueTypeT, ObjIntf, MapFactoryT>
    ::explore_partitions
    (const DivisionsExplorer & explr, const Element & el, int depth) const
{
    using ElementCouple = typename ObjIntf::ElementCouple;
    ElementCouple couple = ObjIntf{}.divide_for_exploration(el, m_division);
    m_low_partition ->explore_partitions(explr, couple.low_side , depth + 1);
    m_high_partition->explore_partitions(explr, couple.high_side, depth + 1);
}

template <typename ValueTypeT, typename ObjIntf, typename MapFactoryT>
/* private */ bool PartitionedSpatialMap<ValueTypeT, ObjIntf, MapFactoryT>::on_low
    (const ValueTypeT & obj) const
{ return only_on_low(ObjIntf{}, obj, m_division); }

template <typename ValueTypeT, typename ObjIntf, typename MapFactoryT>
/* private */ bool PartitionedSpatialMap<ValueTypeT, ObjIntf, MapFactoryT>::on_high
    (const ValueTypeT & obj) const
{ return only_on_high(ObjIntf{}, obj, m_division); }

// ----------------------------------------------------------------------------

template <typename IterType, typename OnLowPred>
void pivot_sort(IterType beg, IterType end, OnLowPred && on_low) {
    // base case: 1 sized sequences are already sorted
    if (end - beg < 2) return;

    auto last = end - 1;
    while (last > beg) {
#       if 0
        // I could do double nested... but this four branches are more readable
        /*  */ if ( on_low(*beg) &&  on_low(*last)) {
            ++beg;
        } else if ( on_low(*beg) && !on_low(*last)) {
            --last;
        } else if (!on_low(*beg) &&  on_low(*last)) {
            std::swap(*beg, *last);
            ++beg;
            --last;
        } else if (!on_low(*beg) && !on_low(*last)) {
            --last;
        }
#       endif
        if (!on_low(*last)) {
            --last;
        } else if (on_low(*beg)) {
            ++beg;
        } else {
            std::swap(*beg, *last);
            ++beg;
            --last;
        }
    }
}

template <typename ValueTypeT, typename ObjIntf>
PivotSortAroundRt<ValueTypeT> pivot_sort_around
    (SpIterator<ValueTypeT> beg, SpIterator<ValueTypeT> end, Real division)
{
    using namespace cul::exceptions_abbr;
    static_assert(std::is_base_of_v<SpatialMapElementGetters<ValueTypeT>, ObjIntf>, "");
    if (!cul::is_real(division)) {
        throw InvArg("pivot_sort_around: division must be a real number.");
    }

    // the STL requires use of a "middle" iterator, which I cannot provide
    // It does not do what I need it to do, and it's time complexity is too
    // great
    pivot_sort(beg, end, [division](const ValueTypeT * obj)
        { return only_on_low(ObjIntf{}, *obj, division); });

    using std::make_reverse_iterator;
    pivot_sort(make_reverse_iterator(beg), make_reverse_iterator(end),
        [division](const ValueTypeT * obj)
        { return only_on_high(ObjIntf{}, *obj, division); });

    PivotSortAroundRt<ValueTypeT> rv;
    auto counts = get_counts<ValueTypeT, ObjIntf>(division, beg, end);
    rv.low_end  = beg + counts.on_low + counts.shared;
    rv.high_beg = beg + counts.on_low;
    return rv;
}

template <typename ValueTypeT, typename ObjIntf>
SpatialMapCounts get_counts
    (Real division, SpConstIterator<ValueTypeT> beg, SpConstIterator<ValueTypeT> end)
{
    static_assert(std::is_base_of_v<SpatialMapElementGetters<ValueTypeT>, ObjIntf>, "");
    auto on_low = [division](const ValueTypeT & obj)
        { return only_on_low(ObjIntf{}, obj, division); };
    auto on_high = [division](const ValueTypeT & obj)
        { return only_on_high(ObjIntf{}, obj, division); };
    SpatialMapCounts rv;
    for (auto itr = beg; itr != end; ++itr) {
        if (!on_low(**itr) && !on_high(**itr)) rv.shared++;
        else if (on_low(**itr)) rv.on_low++;
        else rv.on_high++;
    }
    return rv;
}

inline int sum_counts(const SpatialMapCounts & counts)
    { return counts.on_high + counts.on_low + counts.shared; }

inline bool prefer_lhs_counts
    (const SpatialMapCounts & lhs, const SpatialMapCounts & rhs)
{
    using cul::magnitude;
    //static auto load = [](const SpatialMapCounts & r)
    //    { return r.on_high + r.on_low + r.shared*2; };
    static auto split_power = [](const SpatialMapCounts & r)
        { return std::min(r.on_high, r.on_low); };
    //int left_load  = load(lhs);
    //int right_load = load(rhs);
    int left_split_power = split_power(lhs);
    int righ_split_power = split_power(rhs);
    return left_split_power > righ_split_power;// || left_load < right_load;
}

// ----------------------------------------------------------------------------

template <typename ValueTypeT, typename MapFactoryT>
template <typename IterType>
void SpatialMap<ValueTypeT, MapFactoryT>::set_elements
    (IterType beg, IterType end)
{ set_elements(beg, end, [](IterType itr) -> ValueTypeT * { return &*itr; }); }

template <typename IterType>
constexpr const bool kt_is_random_access_iterator = std::is_same_v<
    typename std::iterator_traits<IterType>::iterator_category,
    std::random_access_iterator_tag>;

template <typename ValueTypeT, typename MapFactoryT>
template <typename IterType, typename Func>
void SpatialMap<ValueTypeT, MapFactoryT>::set_elements
    (IterType beg, IterType end, Func && convert_iterator_to_pointer)
{
    m_pointers.clear();
    if constexpr (kt_is_random_access_iterator<IterType>) {
        m_pointers.reserve(end - beg);
    }
    for (auto itr = beg; itr != end; ++itr) {
        auto cpy = itr;
        static_assert(std::is_same_v<ValueTypeT *, decltype(convert_iterator_to_pointer(cpy))>,
            "conversion function must convert iterator into a pointer.");
        m_pointers.push_back(convert_iterator_to_pointer(cpy));
    }
    m_base = &m_top_level_factory.choose_map_for(m_pointers.begin(), m_pointers.end(), 0);
    m_base->set_elements(m_pointers.begin(), m_pointers.end(), 0);
    // I need a non-recursive way to do this
}

template <typename ValueTypeT, typename MapFactoryT>
SpElementContainer<ValueTypeT> SpatialMap<ValueTypeT, MapFactoryT>
    ::collect_candidates(const Element & el, ElementContainer && cont) const
{
    cont.clear();
    m_base->collect_candidates(el, cont);
    std::sort(cont.begin(), cont.end());
    cont.erase(std::unique(cont.begin(), cont.end()), cont.end());
    return std::move(cont);
}

template <typename ValueTypeT, typename MapFactoryT>
template <typename Func>
void SpatialMap<ValueTypeT, MapFactoryT>
    ::explore_partitions(const Element & el, Func && f) const
{
    struct Inst final : public DivisionsExplorer {
        Inst(Func && f_): f(f_) {}
        void operator () (const Element & el, int depth) const final
            { f(el, depth); }
        Func f;
    };

    explore_partitions_(Inst{std::move(f)}, el);
}

template <typename ValueTypeT, typename MapFactoryT>
/* private */ void SpatialMap<ValueTypeT, MapFactoryT>
    ::explore_partitions_(const DivisionsExplorer & explr, const Element & el) const
{ m_base->explore_partitions(explr, el, 0); }

template <typename ValueTypeT, typename MapFactoryT>
/* private static */ SpatialMapBase<ValueTypeT> &
    SpatialMap<ValueTypeT, MapFactoryT>::default_base()
{
    class Impl final : public SpatialMapBase<ValueTypeT> {
        using SetElIterator = SpIterator<ValueTypeT>;

        void collect_candidates(const Element &, ElementContainer &) const final {}

        void set_elements(SetElIterator, SetElIterator, int) final {
            using namespace cul::exceptions_abbr;
            throw RtError("SpatialMap::default_base()::set_elements: "
                          "this function should never be called (even in "
                          "testing!).");
        }
        void explore_partitions(const DivisionsExplorer &, const Element &, int) const final
            {}
    };
    static_assert(sizeof(SpatialMapBase<ValueTypeT>) == sizeof(Impl),
        "My \"personal coding standard\": shared state should be avoided "
        "if possible, especially at the library level.");
    static Impl inst;
    return inst;
}

} // end of detail namespace -> into ::tdp

} // end of tdp namespace
