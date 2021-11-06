#include "../src/SpatialMapN.hpp"

#include <aabbtdp/defs.hpp>

#include <common/TestSuite.hpp>

#include <array>

namespace {

using cul::ts::TestSuite, cul::ts::Unit, cul::ts::test, cul::ts::set_context,
      cul::bottom_of, cul::right_of,
      tdp::Rectangle, tdp::Real, tdp::Vector;

constexpr const Real k_pi  = cul::k_pi_for_type<Real>;
constexpr const Real k_inf = std::numeric_limits<Real>::infinity();

struct Interval {
    Interval() {}
    Interval(Real low_, Real high_): low(low_), high(high_) {}
    Real low = -k_inf, high = k_inf;
};

} // end of <anonymous> namespace

namespace abpn {

template <>
struct SpatialMapScalar<Rectangle> { using Type = Real; };

template <>
struct SpatialMapScalar<Interval> { using Type = Real; };

} // end of abp(n) namespace

namespace {

using abpn::only_on_high, abpn::only_on_low;

constexpr const int  k_fork_thershold = 8;

class DefaultChecker final : public abpn::SpatialMapRepartitionChecker {
public:
    using Counts = abpn::SpatialMapCounts;

    static constexpr const int k_min_delta = 10;

    static bool should_choose_flat(const Counts & counts) {
        auto sum = counts.high_only + counts.low_only + counts.shared;
        return    counts.shared > k_fork_thershold
               || counts.shared > (sum * 2) / 3;
    }

    bool operator () (const Counts & counts, int delta) const final {
        return should_choose_flat(counts) || delta >= k_min_delta;
    }
};

class IntervalMid : public abpn::SpatialMapKeyGetters<Interval> {
public:
    Real get_low (const Interval & r) const final { return r.low ; }
    Real get_high(const Interval & r) const final { return r.high; }
};

class RectangleDomain : public abpn::SpatialMapKeyGetters<Rectangle> {
public:
    Real domain_min() const final { return -k_inf; }
    Real domain_max() const final { return  k_inf; }
};

class IntervalGetters final : public IntervalMid {
public:
    Real domain_min() const final { return -k_inf; }
    Real domain_max() const final { return  k_inf; }
};

class RadianGetters final : public IntervalMid {
public:
    Real domain_min() const final { return -k_pi; }
    Real domain_max() const final { return  k_pi; }
};

class HorizontalGetters final : public RectangleDomain {
public:
    Real get_low (const Rectangle & r) const final { return r.left     ; }
    Real get_high(const Rectangle & r) const final { return right_of(r); }
};

class VerticalGetters final : public RectangleDomain {
public:
    Real get_low (const Rectangle & r) const final { return r.top       ; }
    Real get_high(const Rectangle & r) const final { return bottom_of(r); }
};

using ValueType = const char *;
using FlatMap1  = abpn::SpatialMapFlat<Interval , ValueType>;
using FlatMap2  = abpn::SpatialMapFlat<Rectangle, ValueType>;

template <typename T>
using UniquePtr = std::unique_ptr<T>;

template <typename KeyT, typename ValueT>
using KvIterator = typename abpn::SpatialKvContainer<KeyT, ValueT>::iterator;// std::vector<std::tuple<const KeyT *, ValueT *>>::iterator;
#if 0
template <typename KeyT, typename ValueT, typename KeyGetters>
std::enable_if_t<std::is_base_of_v<abpn::SpatialMapKeyGetters<KeyT>, KeyGetters>, Real>
    find_division(KeyGetters, KvIterator<KeyT, ValueT> beg, KvIterator<KeyT, ValueT> end)
{
    // beg to end need not be sorted here
    if (end == beg) return 0;
    using abpn::get_key;
    Real avg = 0;
    for (auto itr = beg; itr != end; ++itr) {
        avg += (KeyGetters{}.get_high(get_key(*itr)) + KeyGetters{}.get_low(get_key(*itr))) / 2;
    }
    avg /= Real(end - beg);

    auto get_counts = [beg, end](Real div)
        { return abpn::get_counts<KeyT, ValueT>(KeyGetters{}, beg, end, div); };

    auto mcounts = get_counts(avg);

    auto mid = beg + (end - beg) / 2;
    auto mid_low  = KeyGetters{}.get_low (get_key(*mid));
    auto mid_high = KeyGetters{}.get_high(get_key(*mid));
    auto late_div  = mid_high + (mid_high - mid_low)*0.005;
    auto early_div = mid_low  - (mid_high - mid_low)*0.005;
    auto lcounts = get_counts(late_div );
    auto hcounts = get_counts(early_div);
    using abpn::prefer_lhs_counts;
    // at least five iterations... yuck!
    if (prefer_lhs_counts(mcounts, lcounts) && prefer_lhs_counts(mcounts, hcounts)) {
        return avg;
    }
    return (prefer_lhs_counts(lcounts, hcounts)) ? mid_low : mid_high;
}

template <typename KeyGetters>
class IntervalMapFactory final : public abpn::SpatialMapFactory<Interval, ValueType> {
public:
    static_assert(std::is_base_of_v<abpn::SpatialMapKeyGetters<Interval>, KeyGetters>,
        "KeyGetters type must implement SpatialMapKeyGetters.");

    using PartitionedMap = abpn::SpatialMapPartitioned<Interval, ValueType, IntervalMapFactory<KeyGetters>, KeyGetters>;

    // depth limiting?
    MapBase & ensure_map(KvIterator beg, KvIterator end) final {
        auto div = find_division<Interval, ValueType>(KeyGetters{}, beg, end);
        if (DefaultChecker::should_choose_flat
            (abpn::get_counts(KeyGetters{}, beg, end, div)))
        { return m_flat; }
        if (!m_part) {
            m_part = std::make_unique<PartitionedMap>();
        }
        m_part->set_division(div);
        return *m_part;
    }

private:
    FlatMap1 m_flat;
    UniquePtr<PartitionedMap> m_part;
};

class RectangleMapFactory final : public abpn::SpatialMapFactory<Rectangle, ValueType> {
public:

    using HorizontalMap = abpn::SpatialMapPartitioned<Rectangle, ValueType, RectangleMapFactory, HorizontalGetters>;
    using VerticalMap   = abpn::SpatialMapPartitioned<Rectangle, ValueType, RectangleMapFactory, VerticalGetters  >;

    // depth limiting?
    MapBase & ensure_map(KvIterator beg, KvIterator end) final {
        auto hdiv = find_division<Rectangle, ValueType>(HorizontalGetters{}, beg, end);
        auto vdiv = find_division<Rectangle, ValueType>(VerticalGetters  {}, beg, end);
        auto hcounts = abpn::get_counts<Rectangle, ValueType>(HorizontalGetters{}, beg, end, hdiv);
        auto vcounts = abpn::get_counts<Rectangle, ValueType>(VerticalGetters  {}, beg, end, vdiv);
        if (abpn::prefer_lhs_counts(hcounts, vcounts)) {
            if (DefaultChecker::should_choose_flat(hcounts)) return m_flat;
            return ensure_horz(hdiv);
        }
        if (DefaultChecker::should_choose_flat(vcounts)) return m_flat;
        return ensure_vert(vdiv);
    }

private:
    MapBase & ensure_horz(Real div) {
        if (m_horz) {
            m_horz = std::make_unique<HorizontalMap>();
        }
        m_horz->set_division(div);
        return *m_horz;
    }

    MapBase & ensure_vert(Real div) {
        if (m_vert) {
            m_vert = std::make_unique<VerticalMap>();
        }
        m_vert->set_division(div);
        return *m_vert;
    }

    FlatMap2 m_flat;
    UniquePtr<HorizontalMap> m_horz;
    UniquePtr<VerticalMap  > m_vert;
};
#endif
template <typename KeyT, typename ValueT, std::size_t kt_count>
std::array<std::tuple<const KeyT *, ValueT>, kt_count>
    make_kv_array(std::array<std::tuple<KeyT, ValueT>, kt_count> & inarr)
{
    using std::get;
    std::array<std::tuple<const KeyT *, ValueT>, kt_count> rv;
    for (std::size_t i = 0; i != kt_count; ++i) {
        rv[i] = std::make_tuple(&get<0>(inarr[i]), get<1>(inarr[i]));
    }
    return rv;
}

template <typename KeyT, typename ValueT, std::size_t kt_count>
std::vector<std::tuple<const KeyT *, ValueT>>
    make_kv_vector(const std::array<std::tuple<const KeyT *, ValueT>, kt_count> & arr)
{
    std::vector<std::tuple<const KeyT *, ValueT>> rv;
    rv.reserve(arr.size());
    for (const auto & el : arr) rv.push_back(el);
    return rv;
}

} // end of <anonymous> namespace

#define mark MACRO_MARK_POSITION_OF_CUL_TEST_SUITE

void do_spatial_map_unit_tests_n(TestSuite & suite) {
    using std::make_tuple;
    //suite.start_series("spatial map (new) - only_on_* functions");

    // flat maps have bubbles, so it's important to make sure that it works
    // with that caveat
    //
    // flat maps are generally "just do a linear run" or "just do O(n^2) for n
    // is all"
    suite.start_series("spatial map (new) - flat map");
    mark(suite).test([] {
        using std::get;
        FlatMap1 fm1;
        // has to live on this stack frame
        std::array col = {
            make_tuple(Interval{0, 1}, "hello"),
            make_tuple(Interval{1, 2}, "there"),
            make_tuple(Interval{3, 4}, "world")
        };
        std::vector ptrs = make_kv_vector(make_kv_array(col));
        // let's make sure it's not a coincident
        {
        auto & lhs = get<1>(*(ptrs.begin() + 0));
        auto & rhs = get<1>(*(ptrs.begin() + 1));
        if (lhs < rhs) {
            std::swap(lhs, rhs);
        }
        }

        fm1.set_elements(ptrs.begin(), ptrs.end());
        const char * lhs = get<1>(*(fm1.begin() + 0));
        const char * rhs = get<1>(*(fm1.begin() + 1));
        return test(FlatMap1::ValueTraits::compare(lhs, rhs) < 0);
    });
    // probably should test: find_insertion_point
    mark(suite).test([] {
        using std::get;
        FlatMap1 fm1;
        const char * rem_el = "there";
        std::array col = {
            make_tuple(Interval{0, 1}, "hello"),
            make_tuple(Interval{1, 2}, rem_el),
            make_tuple(Interval{3, 4}, "world")
        };
        std::vector ptrs = make_kv_vector(make_kv_array(col));
        fm1.set_elements(ptrs.begin(), ptrs.end());
        fm1.remove(rem_el);
        struct Inq final : FlatMap1::Inquiry {
            void operator () (const char * s) const final {
                if (!s) encountered_null = true;
            }
            mutable bool encountered_null = false;
        };
        Inq inq;
        fm1.inquiry(Interval{0, 4}, inq);
        return test(inq.encountered_null);
    });
    // test case writing, obviously I want complete coverage... but I should
    // do this on a case by case basis, NOT how the code is written...
    suite.start_series("spatial map (new) - partitioned map (1D)");
    suite.start_series("spatial map (new) - partitioned map (2D)");
}
