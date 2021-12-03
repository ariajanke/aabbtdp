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

#include "helpers.hpp"

#include <cassert>

namespace abpn {

template <typename IterType>
using View = tdp::View<IterType>;

template <typename ValueT>
struct SpatialMapInquiry {
    virtual ~SpatialMapInquiry() {}
    // I'll just pass by copy...
    virtual void operator () (ValueT) const = 0;
};

struct SpatialMapCounts {
    int shared = 0, low_only = 0, high_only = 0;
};

struct SpatialMapRepartitionChecker {
    virtual ~SpatialMapRepartitionChecker() {}
    /// @returns true if the map should repartition its elements
    virtual bool operator () (const SpatialMapCounts &, int delta) const = 0;
};

// must be specialized to reveal scalar type
template <typename KeyT>
struct SpatialMapScalar {
    struct Dummy {
        Dummy() { throw std::runtime_error(""); }
    };

    using Type = Dummy;
};

template <typename ValueT>
struct SpatialMapValueTraits {
    // just a default implementation
    static std::ptrdiff_t compare(const ValueT & a, const ValueT & b)
        { return a - b; }

    static ValueT make_null_sentinel() { return ValueT{}; }
};

template <typename ... Types>
using Tuple = std::tuple<Types...>;

template <typename KeyT, typename ValueT>
using SpatialKvContainer = std::vector<Tuple<const KeyT *, ValueT>>;

template <typename KeyT, typename ValueT>
const KeyT & get_key(const Tuple<const KeyT *, ValueT> & tuple) {
    assert(std::get<0>(tuple));
    return *std::get<0>(tuple);
}

template <typename KeyT, typename ValueT>
ValueT get_value(Tuple<const KeyT *, ValueT> & tuple) {
    return std::get<1>(tuple);
}

template <typename KeyT, typename ValueT>
const ValueT get_value(const Tuple<const KeyT *, ValueT> & tuple) {
    return std::get<1>(tuple);
}

template <typename KeyT, typename ValueT>
class SpatialMapBase {
public:
    using Key           = KeyT;
    using Value         = ValueT;
    using KvContainer   = SpatialKvContainer<Key, Value>;
    using KvIterator    = typename KvContainer::iterator;
    using RepartChecker = SpatialMapRepartitionChecker;
    using Counts        = SpatialMapCounts;

    virtual ~SpatialMapBase() {}

    // this is getting too complicated (for me at least) :c

    // complexities: n = number of total elements
    //               m = number of elements in a inseperable blob

    // ------------------------- individual elements --------------------------

    // O(log m)
    virtual void remove(const Value &) = 0;

    // O(m)
    virtual void update(const Key & old, const Key & new_, const Value &) = 0;

    // ---------------------------- whole container ---------------------------

    // sequence order is changed
    // at least O((end - beg) log(end - beg))
    // this appends a sequence to the map, optionally repartitioning it (or any
    // of its sub containers)
    virtual void append(KvIterator beg, KvIterator end,
                        const RepartChecker &, KvContainer & workspace_cont) = 0;

    // O(n)
    virtual void clear() = 0;

    // O(n)
    virtual void collect_all_elements(KvContainer &) const = 0;

    virtual Counts get_counts() const = 0;

    // gets counts with the addition of element beg and end across this
    // container's divider (or just straight adds it to shared for flats)
    virtual Counts get_counts(KvIterator beg, KvIterator end) const = 0;

    // O(m * log n)
    virtual void inquiry(const Key &, SpatialMapInquiry<Value> &) const = 0;

    // a call explicit to check for repartitioning
    // update calls never trigger a repartition, thus this method exists
    virtual void recheck_structure(KvContainer &, const RepartChecker &) = 0;

    // this turned out to be necessary, but it does not necessarily need to be
    // called every frame
    // O(n log n)
    virtual void set_elements(KvIterator beg, KvIterator end) = 0;
};

template <typename KeyT, typename ValueT>
class SpatialMapFlat final : public SpatialMapBase<KeyT, ValueT> {
public:
    using Key           = KeyT;
    using Value         = ValueT;
    using KvContainer   = SpatialKvContainer<Key, Value>;
    using Inquiry       = SpatialMapInquiry<Value>;
    using KvIterator    = typename KvContainer::iterator;
    using RepartChecker = SpatialMapRepartitionChecker;
    using Counts        = SpatialMapCounts;
    using ValueTraits   = SpatialMapValueTraits<ValueT>;

    void remove(const Value & val) final {
        KvIterator itr;
        InsertType ins_type;
        std::tie(ins_type, itr) = find_insertion_point(m_elements.begin(), m_elements.end(), val);
        if (ins_type != k_is_present) return;
        *itr = std::make_tuple(nullptr, ValueTraits::make_null_sentinel());
        check_invarients();
    }

    // reappend in O(n) time? (or O(n^2) for sequence of n?)
    void update(const Key &, const Key & new_, const Value & val) final {
        KvIterator itr;
        InsertType ins_type;
        std::tie(ins_type, itr) = find_insertion_point(m_elements.begin(), m_elements.end(), val);
        // which key should I use?
        switch (ins_type) {
        case k_is_present: return;
        case k_needs_to_insert:
            m_elements.insert(itr, std::make_tuple(&new_, val));
            break;
        case k_on_bubble:
            *itr = std::make_tuple(&new_, val);
            break;
        }
        check_invarients();
    }

    // ------------------------------------------------------------------------

    // two params ignored, no further subcontainer could possible
    void append(KvIterator beg, KvIterator end,
                const RepartChecker &, KvContainer &) final
    {
        // we're already O(m) here
        for (auto itr = beg; itr != end; ++itr) {
            m_elements.push_back(*itr);
        }
        clean_up_nulls();
        std::sort(m_elements.begin(), m_elements.end(), compare_elements);
        auto uend = std::unique(m_elements.begin(), m_elements.end());
        m_elements.erase(uend, m_elements.end());
        check_invarients();
    }

    void clear() final {
        m_elements.clear();
        check_invarients();
    }

    void collect_all_elements(KvContainer & cont) const final {
        cont.insert(cont.end(), m_elements.begin(), m_elements.end());
    }

    Counts get_counts() const final {
        Counts rv;
        // imposes a hard limit of ~2.1bil elements
        rv.shared = int(m_elements.size());
        return rv;
    }

    Counts get_counts(KvIterator beg, KvIterator end) const final {
        auto rv = get_counts();
        assert(end >= beg);
        rv.shared += (end - beg);
        return rv;
    }

    void inquiry(const Key &, Inquiry & inq) const final {
        for (const auto & tuple : m_elements) {
            // check for nullptr intentionally omitted
            inq(get_value(tuple));
        }
    }

    void recheck_structure(KvContainer &, const RepartChecker &) final {
        clean_up_nulls();
    }

    void set_elements(KvIterator beg, KvIterator end) final {
        // check elements that they are clear of nulls, throw if otherwise!
        assert(std::all_of(beg, end, [](const KvElement & el) { return !is_bubble(el); }));
        m_elements.clear();
        m_elements.insert(m_elements.begin(), beg, end);
        std::sort(m_elements.begin(), m_elements.end(), compare_elements);
        check_invarients();
    }

    // --------------------- methods present for testing ----------------------
    // information revealed should not be harmful, though it shouldn't be
    // relied upon

    typename KvContainer::const_iterator begin() const { return m_elements.begin(); }

    typename KvContainer::const_iterator end() const { return m_elements.end(); }

    // sets elements without sorting or checking for bubbles
    // (invarients still must be maintained)
    void set_elements_min(KvIterator beg, KvIterator end) {
        m_elements.insert(m_elements.begin(), beg, end);
        check_invarients();
    }

    // I need... whether the element is new or not, if not is this iterator at
    // a bubble, or do I need to do an insert?
    enum InsertType {
        k_needs_to_insert, // not present, needs insertion
        k_on_bubble, // not present, may replace element at this iterator
        k_is_present // is present and *at* this iterator
    };


    // insertion point should not alter the order of the container
    // This can either be O(log n) or O(n) depending on the presence of 'bubbles'
    static Tuple<InsertType, KvIterator>
        find_insertion_point(KvIterator beg, KvIterator end, const Value & val)
    {
        using std::make_tuple, std::get;
        // must be handled...
        if (end - beg == 1) {
            if (is_bubble(*beg)) return make_tuple(k_on_bubble, beg);
            // further cases are handled by another call
        }
        auto beg_ = walk_foreward_nulls(beg , end);
        return find_insertion_point_impl(beg_, walk_back_nulls(beg_, end), val);
    }

private:
    static Tuple<InsertType, KvIterator>
        find_insertion_point_impl(KvIterator beg, KvIterator end, const Value & val)
    {
        using std::make_tuple, std::get;
        if (end - beg == 0) {
            return make_tuple(k_needs_to_insert, end);
        } else if (end - beg == 1) {
            if (is_bubble(*beg)) throw std::runtime_error("???");
            auto cmpres = ValueTraits::compare(get<1>(*beg), val);
            if (cmpres == 0)
                return make_tuple(k_is_present, beg);
            else if (cmpres > 0)
                return make_tuple(k_needs_to_insert, beg);
            else if (cmpres < 0)
                throw std::runtime_error("I think we're in trouble here...");
        }

        // it is possible the entire sequence is bubbles, thus the this
        // algorithm would grow to O(n)

        if (!is_in(beg, end, val)) {
            if (beg == end) return make_tuple(k_needs_to_insert, end);
            auto ins_type = is_bubble(*(end - 1)) ? k_on_bubble : k_needs_to_insert;
            return make_tuple(ins_type, end);
        }

        auto low_end  = walk_back_nulls    (beg + (end - beg) / 2, end);
        auto high_beg = walk_foreward_nulls(beg + (end - beg) / 2, end);
        if (is_in(beg, low_end, val)) {
            return find_insertion_point_impl(beg, low_end, val);
        } else if (is_in(high_beg, end, val)) {
            return find_insertion_point_impl(high_beg, end, val);
        }
        throw std::runtime_error("bad branch");
    }

private:
    using KvElement = typename KvContainer::value_type;

    static bool compare_elements(const KvElement & lhs, const KvElement & rhs)
        { return ValueTraits::compare(get_value(lhs), get_value(rhs)) < 0; }

    static bool compare_el_to_value(const KvElement & lhs, const Value & val)
        { return ValueTraits::compare(get_value(lhs), val) < 0; }

    static bool is_bubble(const KvElement & el)
        { return !std::get<0>(el); }

    void clean_up_nulls() {
        auto itr = std::remove_if(m_elements.begin(), m_elements.end(), is_bubble);
        m_elements.erase(itr, m_elements.end());
        check_invarients();
    }

    // nullptr tuples are an exception and maybe anywhere!
    void check_invarients() const {
        // I think this will work on non strictly weak ordered elements?
        static auto ordered_except_null = []
            (const KvElement & lhs, const KvElement & rhs)
        {
            if (is_bubble(lhs) || is_bubble(rhs)) return true;
            return compare_elements(lhs, rhs);
        };
        assert(std::is_sorted(m_elements.begin(), m_elements.end(), ordered_except_null));
    }

    static bool is_in(KvIterator beg, KvIterator end, const ValueT & val) {
        if (beg == end) return false;
        assert(!is_bubble(*beg) && !is_bubble(*(end - 1)));
        return    ValueTraits::compare(val, std::get<1>(*beg)) >= 0
               && ValueTraits::compare(std::get<1>(*(end - 1)), val) <= 0;
    }

    static KvIterator walk_back_nulls(KvIterator beg, KvIterator end) {
        while (beg != end) {
            if (is_bubble(*(end - 1))) --end;
            else break;
        }
        return end;
    }

    static KvIterator walk_foreward_nulls(KvIterator beg, KvIterator end) {
        while (beg != end) {
            if (is_bubble(*beg)) ++beg;
            else break;
        }
        return beg;
    }

    KvContainer m_elements;
};

template <typename KeyT, typename ValueT>
class SpatialMapFactory {
public:
    using MapBase    = SpatialMapBase<KeyT, ValueT>;
    using KvIterator = typename SpatialKvContainer<KeyT, ValueT>::iterator;

    // this needs to do division computations
    // (and should handle empty sequences!)
    virtual MapBase & ensure_map(KvIterator beg, KvIterator end) = 0;
};

template <typename KeyT>
class SpatialMapKeyGetters {
public:
    using Scalar = typename SpatialMapScalar<KeyT>::Type;
    using Key    = KeyT;
    virtual Scalar get_low(const Key &) const = 0;
    virtual Scalar get_high(const Key &) const = 0;

    virtual Scalar domain_min() const = 0;
    virtual Scalar domain_max() const = 0;
};

template <typename Base, typename A, typename Rt>
using EnableBaseOf = std::enable_if_t<std::is_base_of_v<Base, A>, Rt>;

// This test if you're *strictly* on low
// if the element is neither, then it's shared
template <typename KeyGettersT, typename KeyT>
EnableBaseOf<SpatialMapKeyGetters<KeyT>, KeyGettersT, bool>
    only_on_low(KeyGettersT, const KeyT & key, typename KeyGettersT::Scalar division)
{
    using Scalar = typename KeyGettersT::Scalar;
    KeyGettersT inst{};
    bool behind_div = inst.get_high(key) < division;
    if (!behind_div) return false;

    Scalar domain_min = inst.domain_min();
    Scalar low        = inst.get_low(key);
    return low >= domain_min && low < division;
}

// This test if you're *strictly* on high
// if the element is neither, then it's shared
template <typename KeyGettersT, typename KeyT>
EnableBaseOf<SpatialMapKeyGetters<KeyT>, KeyGettersT, bool>
    only_on_high(KeyGettersT, const KeyT & key, typename KeyGettersT::Scalar division)
{
    using Scalar = typename KeyGettersT::Scalar;
    KeyGettersT inst{};
    bool ahead_of_div = inst.get_low(key) >= division;
    if (!ahead_of_div) return false;

    Scalar domain_max = inst.domain_max();
    Scalar high       = inst.get_high(key);
    return high <= domain_max && high > division;
}

// O(n log n)
template <typename KeyT, typename ValueT>
void dedupelicate_container(SpatialKvContainer<KeyT, ValueT> & cont) {
    using Couple = typename SpatialKvContainer<KeyT, ValueT>::value_type;
    using Traits = SpatialMapValueTraits<ValueT>;
    std::sort(cont.begin(), cont.end(), [](const Couple & lhs, const Couple & rhs) {
        return Traits::compare(get_value(lhs), get_value(rhs)) < 0;
    });
    auto uend = std::unique(cont.begin(), cont.end(),
        [](const Couple & lhs, const Couple & rhs)
    { return Traits::compare(get_value(lhs), get_value(rhs)) == 0; });
    cont.erase(uend, cont.end());
}

// O(n)
template <typename IterType, typename OnLowPred>
void incomplete_sort(IterType beg, IterType end, OnLowPred && on_low) {
    // base case: 1 sized sequences are already sorted
    if (end - beg < 2) return;

    for (auto last = end - 1; last > beg; ) {
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

template <typename KeyT, typename ValueT, typename KeyGetters>
EnableBaseOf<SpatialMapKeyGetters<KeyT>, KeyGetters, SpatialMapCounts>
    get_counts
    (KeyGetters,
     typename SpatialKvContainer<KeyT, ValueT>::iterator beg,
     typename SpatialKvContainer<KeyT, ValueT>::iterator end,
     typename abpn::SpatialMapScalar<KeyT>::Type div)
{
    using namespace abpn;
    SpatialMapCounts counts;
    for (auto itr = beg; itr != end; ++itr) {
        if (only_on_low(KeyGetters{}, get_key(*itr), div)) {
            ++counts.low_only;
        } else if (only_on_high(KeyGetters{}, get_key(*itr), div)) {
            ++counts.high_only;
        } else {
            ++counts.shared;
        }
    }
    return counts;
}

inline bool prefer_lhs_counts
    (const SpatialMapCounts & lhs, const SpatialMapCounts & rhs)
{
    using cul::magnitude;
    //static auto load = [](const SpatialMapCounts & r)
    //    { return r.on_high + r.on_low + r.shared*2; };
    static auto split_power = [](const SpatialMapCounts & r)
        { return std::min(r.high_only, r.low_only); };
    //int left_load  = load(lhs);
    //int right_load = load(rhs);
    int left_split_power = split_power(lhs);
    int righ_split_power = split_power(rhs);
    return left_split_power > righ_split_power;// || left_load < right_load;
}

inline int delta_between(const SpatialMapCounts & old, const SpatialMapCounts & new_) {
    using cul::magnitude;
    return   magnitude(old.high_only - new_.high_only)
           + magnitude(old.low_only  - new_.low_only )
           + magnitude(old.shared    - new_.shared   );
}

template <typename KeyT, typename ValueT, typename MapFactory, typename KeyGetters>
class SpatialMapPartitioned final : public SpatialMapBase<KeyT, ValueT> {
public:
    using Key           = KeyT;
    using Value         = ValueT;
    using Scalar        = typename SpatialMapScalar<KeyT>::Type;
    using KvContainer   = std::vector<std::tuple<const Key *, Value *>>;
    using KvIterator    = typename KvContainer::iterator;
    using Inquiry       = SpatialMapInquiry<Value>;
    using RepartChecker = SpatialMapRepartitionChecker;
    using Counts        = SpatialMapCounts;

    static_assert(std::is_base_of_v<SpatialMapFactory<KeyT, ValueT>, MapFactory>,
        "MapFactory type must implement SpatialMapFactory.");

    static_assert(std::is_base_of_v<SpatialMapKeyGetters<KeyT>, KeyGetters>,
        "KeyGetters type must implement SpatialMapKeyGetters.");

    // update, requesting_repartitioning, and append needs finishing
    // should this keep track of shared, low only, and high only?

    void remove(const Value & val) final {
        m_low ->remove(val);
        m_high->remove(val);
    }

    // this sort of presupposes the existence of the element...
    // though this may absolutely remove a value from one sub container and
    // move it to another, or start sharing it between multiple sub containers
    void update(const Key & old, const Key & new_, const Value & val) final {
        update_side<k_low_side >(old, new_, val);
        update_side<k_high_side>(old, new_, val);
        update_counts(old, new_);
        check_invarients();
    }

    // ------------------------------------------------------------------------

    void append(KvIterator beg, KvIterator end,
                const RepartChecker & needs_to_repart, KvContainer & workspace_cont) final
    {
        if (beg == end) return;
        auto divs = incomplete_sort_with_interval(beg, end);

        append_to_side<k_low_side >(beg,  divs.low_end, needs_to_repart, workspace_cont);
        append_to_side<k_high_side>(divs.high_beg, end, needs_to_repart, workspace_cont);

        check_invarients();
    }

    void clear() final {
        m_low ->clear();
        m_high->clear();
    }

    void collect_all_elements(KvContainer & container) const {
        m_low ->collect_all_elements(container);
        m_high->collect_all_elements(container);
    }

    Counts get_counts() const final { return m_counts; }

    Counts get_counts(KvIterator beg, KvIterator end) const final {
        auto count = m_counts;
        for (auto itr = beg; itr != end; ++itr) {
            const auto & key = get_key(*itr);
            if (only_on_low_(key)) {
                ++count.low_only;
            } else if (only_on_high_(key)) {
                ++count.high_only;
            } else {
                ++count.shared;
            }
        }
        return count;
    }

    void inquiry(const Key & key, Inquiry & inq) const final {
        assert(!only_on_low_(key) || !only_on_high_(key));
        if (!only_on_low_(key)) {
            m_high->inquiry(key, inq);
        }
        if (!only_on_high_(key)) {
            m_low->inquiry(key, inq);
        }
    }

    void recheck_structure(KvContainer & cont, const RepartChecker & checker) final {
        if (   recheck_structure(m_low , m_low_factory , m_element_delta, cont, checker)
            || recheck_structure(m_high, m_high_factory, m_element_delta, cont, checker))
        {
            // delta reset upon repartitioning either side
            m_element_delta = 0;
        }
        check_invarients();
    }

    void set_elements(KvIterator beg, KvIterator end) final {
        m_low ->clear();
        m_high->clear();
        auto divs = incomplete_sort_with_interval(beg, end);
        m_low ->set_elements(beg          , divs.low_end);
        m_high->set_elements(divs.high_beg, end         );
        m_element_delta = 0;
        check_invarients();
    }

    // ------------------------------------------------------------------------

    void set_division(const Scalar & r) {
        KeyGetters inst{};
        using namespace cul::exceptions_abbr;
        if (r >= inst.domain_max() || r <= inst.domain_min()) {
            throw InvArg("SpatialMapPartitioned::set_division: division must "
                         "between and not equal to either boundry.");
        } else if constexpr (std::is_floating_point_v<Scalar>) {
            if (cul::is_nan(r)) {
                throw InvArg("SpatialMapPartitioned::set_division: division "
                             "must not be \"not any number\".");
            }
        }
        m_division = r;
    }

    // revealed for testing, should not be too harmful
    int current_delta() const { return m_element_delta; }

private:
    using MapBase = SpatialMapBase<KeyT, ValueT>;
    enum Side { k_low_side, k_high_side, k_shared };

    struct KvLowHighDiv {
        KvIterator low_end, high_beg;
    };

    bool only_on_low_(const Key & ky) const { return only_on_low(KeyGetters{}, ky, m_division); }

    bool only_on_high_(const Key & ky) const { return only_on_high(KeyGetters{}, ky, m_division); }

    // the front map layer is going to behave very similarly
    /// @returns true if the partition was "repartitioned"
    static bool recheck_structure
        (MapBase *& part, MapFactory & factory, int delta,
         KvContainer & workspace_cont, const RepartChecker & needs_to_repart)
    {
        // need a minimum delta here also
        if (needs_to_repart( part->get_counts(), delta )) {
            // check if we should honor the request...
            // that is: check if blob is seperable, then repartition
            // blobs will always reqest seperation though...

            workspace_cont.clear();
            part->collect_all_elements(workspace_cont);
            // O(n log n)
            dedupelicate_container(workspace_cont);
            part->clear();
            part = &factory.ensure_map(workspace_cont.begin(), workspace_cont.end());
            // now... how should I add them back?
            part->set_elements(workspace_cont.begin(), workspace_cont.end());
            return true;
        }
        part->recheck_structure(workspace_cont, needs_to_repart);
        return false;
    }

    // :TODO: make sure this accounts for everything that needs to be done for
    // a side
    template <Side kt_side>
    void append_to_side(KvIterator beg, KvIterator end,
                        const RepartChecker & needs_to_repart, KvContainer & workspace_cont)
    {
        // beg to end are assumed:
        // - to be sorted
        // - to contain *only* low side elements and
        //   shared elements
        static_assert(kt_side != k_shared, "no");
        static constexpr const bool k_is_low = kt_side == k_low_side;
        assert(([this, beg, end] {
            for (auto itr = beg; itr != end; ++itr) {
                if (side_of(get_key(*itr)) != kt_side) return false;
            }
            return true;
        } ()));

        MapBase    & map     = *(k_is_low ? m_low : m_high);
        MapFactory & factory = k_is_low ? m_low_factory : m_high_factory;

        auto counts = map.get_counts(beg, end);
        // how much old delta is on this side?
        if (needs_to_repart(counts, end - beg)) {
            workspace_cont.clear();
            map.collect_all_elements(workspace_cont);
            workspace_cont.insert(workspace_cont.end(), beg, end);
            dedupelicate_container(workspace_cont);
            // If I can run through two sequences, that won't be so bad...
            // Is there a better way to handle sequences?
            // I have to handle both append and ensure_map
            // a solution that seems relatively simple: use a vector of workspace vectors
            // ...even if I could use "polymorphic iterators" or do sequences
            //    seperately, I still need a vector of vectors to accomodate
            //    further workspaces
            map = factory.ensure_map(workspace_cont.begin(), workspace_cont.end());
            // important note: elements only live in flats
            map.set_elements(workspace_cont.begin(), workspace_cont.end());
        } else {
            map.append(beg, end, needs_to_repart, workspace_cont);
            m_element_delta += delta_between(m_counts, counts);
        }
        m_counts = counts;
    }

    template <Side kt_side>
    void update_side(const Key & old, const Key & new_, const Value & val) {
        static_assert(kt_side != k_shared, "no");
        static constexpr const bool on_low = kt_side == k_low_side;
        bool old_in = !(on_low ? only_on_high_(old ) : only_on_low_(old ));
        bool new_in = !(on_low ? only_on_high_(new_) : only_on_low_(new_));
        auto * part = on_low ? m_low : m_high;
        if (old_in && new_in) {
            part->update(old, new_, val);
        } else if (old_in && !new_in) {
            part->remove(val);
            ++m_element_delta;
        } else if (!old_in && new_in) {
            // let's allow update to add...
            // and do insertion sort... updates would make it a O(n (log n)^2)
            // [I think?]
            part->update(old, new_, val);
            ++m_element_delta;
        } else {
            // never on this side
        }
        // keep counts?
    }

    void update_counts(const Key & old, const Key & new_) {
        switch (side_of(old)) {
        case k_high_side: --m_counts.high_only;
        case k_low_side : --m_counts.low_only ;
        case k_shared   : --m_counts.shared   ;
        }
        switch (side_of(new_)) {
        case k_high_side: ++m_counts.high_only;
        case k_low_side : ++m_counts.low_only ;
        case k_shared   : ++m_counts.shared   ;
        }
    }

    Side side_of(const Key & ky) const {
        if (only_on_high_(ky)) return k_high_side;
        if (only_on_low_ (ky)) return k_low_side ;
        return k_shared;
    }

    KvLowHighDiv incomplete_sort_with_interval(KvIterator beg, KvIterator end) const {
        using KvElement = typename KvContainer::value_type;
        using std::make_reverse_iterator;
        incomplete_sort(beg, end, [this](const KvElement & tuple)
            { return only_on_low_(get_key(tuple)); });
        incomplete_sort(beg, end, [this](const KvElement & tuple)
            { return only_on_high_(get_key(tuple)); });
        // still need to append to sub containers (need that "shared" interval)
        auto low_end = beg;
        for (; only_on_low_(get_key(*low_end)); ++low_end) {}
        auto high_beg = low_end;
        for (; !only_on_high_(get_key(*low_end)); ++low_end) {}
        KvLowHighDiv rv;
        rv.low_end = low_end;
        rv.high_beg = high_beg;
        return rv;
    }

    void check_invarients() const {
        // assert(m_counts.low_only + m_counts.shared == );
        assert(m_element_delta >= 0);
    }

    MapBase * m_low = nullptr, * m_high = nullptr;
    SpatialMapCounts m_counts;
    int m_element_delta = 0;
    Scalar m_division = KeyGetters{}.domain_max();
    MapFactory m_low_factory, m_high_factory;
};

// ----------------------------------------------------------------------------

template <typename KeyT, typename ValueT, typename MapFactory>
class SpatialMap {
public:
    using Key   = KeyT;
    using Value = ValueT;

    static_assert(std::is_base_of_v<SpatialMapFactory<KeyT, ValueT>, MapFactory>,
        "MapFactory type must implement SpatialMapFactory.");

    void append(Key *, Value *);

    void update(const Key &, Value *);

    void remove(Value *);

    // there is no deduplication/lack of bubbles guarentee
    template <typename Func>
    void inquiry(const Key &, Func &&) const;

    void recheck_structure();

private:
};

} // end of abp(n) namespace
