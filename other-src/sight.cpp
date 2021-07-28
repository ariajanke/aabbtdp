#include "demo.hpp"

#include <common/SubGrid.hpp>

// ideas describing this are quite complex
class Sighting {
public:
    struct Entry {
        ecs::EntityRef entity;
        Real opacity;
        Rectangle bounds;
    };

    struct Percept {
        ecs::EntityRef entity;
        Real average_visibility = 0;
        Real visible_length = 0;
        // distance maybe zero for entries which this source occupies
        Real distance = 0;
    };

    virtual ~Sighting() {}

    // going to start with other AABBs first
    virtual void add_entry(const Entry &) = 0;

    // this can very easily turn into O(n^2), but I'm trying to make an
    // implementation that avoid it (try to reach O(n log n))
    //
    // In game there are many ways I can limit the number of entries sent
    //

    // This function does mutates the state of this Sighting Handler.
    // This call makes this handler ready for more entries to be added for
    // additional calls later
    virtual const std::vector<Percept> & operator ()
        (Vector source, Vector facing, Real splay_angle) = 0;
};

// ----------------------------------------------------------------------------

#include <common/Vector2Util.hpp>

struct PolarVector {
    Real r = 0, theta = 0;
};

inline bool are_same(const PolarVector & lhs, const PolarVector & rhs)
    { return lhs.r == rhs.r && lhs.theta == rhs.theta; }

inline bool operator == (const PolarVector & lhs, const PolarVector & rhs)
    { return are_same(lhs, rhs); }

inline bool operator != (const PolarVector & lhs, const PolarVector & rhs)
    { return !are_same(lhs, rhs); }

// this exists to enforce and interface
template <typename T>
struct PartitionMapObjectInterface {
    virtual Real get_low(const T &) const = 0;
    virtual Real get_high(const T &) const = 0;
    virtual void set_mark(T &, bool is_marked) const = 0;
    virtual bool is_marked(const T &) const = 0;

};

template <typename T, typename ObjIntf>
class PartitionMap final {
public:
    static_assert(std::is_base_of_v<PartitionMapObjectInterface<T>, ObjIntf>, "");

    // like a greedy algorithm
    // there maybe many shares between buckets
    static constexpr const std::size_t k_bucket_limit = 12;

    // should be O(n log n)
    template <typename IterType>
    void set_elements(IterType beg, IterType end) {

    }

    template <typename Func>
    void for_each_within(Real low, Real high, Func &&) const;

private:
    static Real get_low(const T & obj) { return ObjIntf{}.get_low(obj); }

    static Real get_high(const T & obj) { return ObjIntf{}.get_high(obj); }

    static void set_mark(T & obj, bool is_marked)
        { ObjIntf{}.set_mark(obj, is_marked); }

    static bool is_marked(const T & obj) { return ObjIntf{}.is_marked(obj); }
};

class SightingComplete final : public Sighting {
public:
    using EntityRef = ecs::EntityRef;
    void add_entry(const Entry &) final;

    const std::vector<Percept> & operator () (Vector source, Vector facing, Real splay_angle) final;

    using OpacityGrid = cul::ConstSubGrid<Real>;

    struct ImageEntry {
        EntityRef entity;
        // always real
        PolarVector low, high;
    };

    using EntryVec = std::vector<ImageEntry>;

private:
    // input
    std::vector<Entry> m_preentries;

    // output
    std::vector<Percept> m_percepts;

    void clear_working_containers();

    // try to restrain to O(log n) since this is in a O(n) loop
    Percept find_percept_of(Vector source, Real low, Real high,
                            const ImageEntry &) const;

    // any "workspace" variables
    std::vector<const ImageEntry *> m_entries_by_hash;
    std::vector<const ImageEntry *> m_entries_low, m_entries_high;
    std::vector<ImageEntry> m_entries;
};

namespace {

static constexpr const Real k_pi = cul::k_pi_for_type<Real>;
static constexpr const Real k_very_close = 0.05; // this is *not* an error quantity
static const Vector k_anchor = Vector{ std::cos(0), std::sin(0) };

using ImageEntry = SightingComplete::ImageEntry;
using Percept = SightingComplete::Percept;
using Entry = SightingComplete::Entry;
template <typename T>
/* portmanteau of "varriable array" like "vary" */ using Varray = std::vector<T>;
using cul::rotate_vector, cul::directed_angle_between;

// how do I unit test these things?

Real angle_from_anchor(const Vector &);

// mmm... what would be a better temporary name for "vector"

void make_images(const Varray<Entry> &, Varray<ImageEntry> &);

void cut_intersections(Varray<ImageEntry> &);

void sort_by_hash(const Varray<ImageEntry> &, Varray<const ImageEntry *> &);

ImageEntry & get_entry(Varray<const ImageEntry *> &, std::size_t hash);

void sort_by_low(Varray<const ImageEntry *> &);

void sort_by_high(Varray<const ImageEntry *> &);

} // end of <anonymous> namespace

void SightingComplete::add_entry(const Entry & entry) {
    //m_entries.push_back(entry);
}

const std::vector<Sighting::Percept> & SightingComplete::operator ()
    (Vector source, Vector facing, Real splay_angle)
{
    clear_working_containers();
    make_images(m_preentries, m_entries);
    sort_by_hash(m_entries, m_entries_by_hash);
    m_entries_low = m_entries_high = m_entries_by_hash;
    sort_by_low(m_entries_low);
    sort_by_high(m_entries_high);

    Real low = angle_from_anchor(rotate_vector(facing, -splay_angle));
    Real high = angle_from_anchor(rotate_vector(facing, splay_angle));
    for (auto & entry : m_entries) {
        m_percepts.emplace_back(find_percept_of(source, low, high, entry));
    }

    // on exit stuff
    m_entries.clear();
    return m_percepts;
}

/* private */ void SightingComplete::clear_working_containers() {
    m_entries.clear();
    m_entries_by_hash.clear();
}

/* private */ Percept SightingComplete::find_percept_of
    (Vector source, Real low, Real high,
     const ImageEntry & image) const
{
    // an image that encloses the source will have a high and low point equal
    // to each other, and a "very small" distance
    //
    // by doing it this we can ensure that they are processed correctly

    // have to check to be sure that my choice of *_bound algorithm makes sense
    static auto comp_to_low_angle = [](const ImageEntry * image, Real val)
        { return image->low.theta < val; };
    // this isn't enough here... I need the other side of this range...
    auto lc_beg = std::lower_bound(m_entries_low.begin(), m_entries_low.end(), low,
                     comp_to_low_angle);
    // is it really end? or last?
    // especially tricky with floating points!
    auto lc_end = std::upper_bound(m_entries_low.begin(), m_entries_low.end(), high,
                     comp_to_low_angle);

    // make sure to only check obstruction only once per other image
}

namespace {

Real angle_from_anchor(const Vector & r) {
    auto angle = directed_angle_between(k_anchor, r);
    return (angle < 0) ? k_pi*2 - angle : angle;
}

} // end of <anonymous> namespace
