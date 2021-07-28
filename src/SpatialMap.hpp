#pragma once

#include <aabbtdp/physics.hpp>

namespace tdp {

namespace detail {

template <typename ValueTypeT>
using SpElementContainer = std::vector<ValueTypeT *>;

template <typename ValueType>
using SpIterator = typename std::vector<ValueType *>::iterator;

template <typename ValueType>
using SpConstIterator = typename std::vector<ValueType *>::const_iterator;

template <typename ValueTypeT>
class SpatialMapBase {
public:
    virtual ~SpatialMapBase() {}
    // type is not visible in the "back"
    // I think I'm going to reintroduce templates...
    using Element          = ValueTypeT;
    using SetElIterator    = SpIterator<ValueTypeT>;
    using ElementContainer = SpElementContainer<ValueTypeT>;

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
};

template <typename ValueTypeT>
class FlatSpatialMap final : public SpatialMapBase<ValueTypeT> {
public:
    ~FlatSpatialMap() final {}

    using Element          = ValueTypeT;
    using SetElIterator    = SpIterator<ValueTypeT>;
    using ElementContainer = SpElementContainer<ValueTypeT>;

    void set_elements(SetElIterator beg, SetElIterator end, int) final;

    void collect_candidates(const Element &, ElementContainer &) const final;

private:
    ElementContainer m_container;
};

template <typename ValueTypeT>
struct SpatialMapElementGetters {
    using MapBase = SpatialMapBase<ValueTypeT>;
    virtual Real get_low (const ValueTypeT &) const = 0;
    virtual Real get_high(const ValueTypeT &) const = 0;
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

    using Element          = ValueTypeT;
    using SetElIterator    = SpIterator<ValueTypeT>;
    using ElementContainer = SpElementContainer<ValueTypeT>;

    // at this point we've already figured out that we want this partition map
    void set_elements(SetElIterator beg, SetElIterator end, int depth) final;

    // so we should know the division we want too?
    void set_division(Real);

    void collect_candidates(const Element &, ElementContainer &) const final;

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

    using Element          = ValueTypeT;
    using ElementContainer = SpElementContainer<ValueTypeT>;

    template <typename IterType>
    void set_elements(IterType beg, IterType end);

    ElementContainer collect_candidates(const Element &, ElementContainer && = ElementContainer()) const;

private:
    static SpatialMapBase<Element> & default_base();

    using MapBase = SpatialMapBase<Element>;
    std::vector<Element *> m_pointers;
    MapFactoryT m_top_level_factory;
    MapBase * m_base = &default_base();
};

// This test if you're *strictly* on low
// if the element is neither, then it's shared
template <typename ObjIntf, typename ValueTypeT>
bool only_on_low(ObjIntf, const ValueTypeT & obj, Real division) {
    static_assert(std::is_base_of_v<SpatialMapElementGetters<ValueTypeT>, ObjIntf>, "");
    return ObjIntf{}.get_high(obj) < division;
}

// This test if you're *strictly* on high
// if the element is neither, then it's shared
template <typename ObjIntf, typename ValueTypeT>
bool only_on_high(ObjIntf, const ValueTypeT & obj, Real division) {
    static_assert(std::is_base_of_v<SpatialMapElementGetters<ValueTypeT>, ObjIntf>, "");
    return ObjIntf{}.get_low(obj) >= division;
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

// ----------------------------------------------------------------------------

template <typename ValueTypeT, typename ObjIntf, typename MapFactoryT>
void PartitionedSpatialMap<ValueTypeT, ObjIntf, MapFactoryT>::
    set_elements(SetElIterator beg, SetElIterator end, int depth)
{
    // will need to partial sort here...
    // partially_sort_around will throw if m_division is not a real number
    auto gv = pivot_sort_around<Element, ObjIntf>(beg, end, m_division);

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
void PartitionedSpatialMap<ValueTypeT, ObjIntf, MapFactoryT>::
    collect_candidates(const Element & element, ElementContainer & outcont) const
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

// ----------------------------------------------------------------------------

template <typename ValueTypeT, typename MapFactoryT>
template <typename IterType>
void SpatialMap<ValueTypeT, MapFactoryT>::set_elements
    (IterType beg, IterType end)
{
    m_pointers.clear();
    m_pointers.reserve(end - beg);
    for (auto itr = beg; itr != end; ++itr) {
        static_assert(std::is_same_v<ValueTypeT *, decltype(&*itr)>,
            "must be convertible to pointer");
        m_pointers.push_back(&*itr);
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
    };
    static_assert(sizeof(SpatialMapBase<ValueTypeT>) == sizeof(Impl),
        "My \"personal coding standard\": shared state should be avoided "
        "if possible, especially at the library level.");
    static Impl inst;
    return inst;
}

} // end of detail namespace -> into ::tdp

} // end of tdpn
