#pragma once

#include <aabbtdp/sight.hpp>

#include "../src/SpatialMap.hpp"

#include <common/Vector2Util.hpp>

#include <variant>

namespace tdp {

namespace detail {

struct PolarVector {
    PolarVector() {}
    PolarVector(Real r_, Real th_):
        r(r_), theta(th_) {}
    explicit PolarVector(const Vector & r_):
        r    (cul::magnitude(r_)),
        theta(std::atan2(r_.y, r_.x))
    {}
    Real r = 0, theta = 0;
};

inline Vector to_cartesian(const PolarVector & v)
    { return Vector{v.r*std::cos(v.theta), v.r*std::sin(v.theta)}; }

struct ImageEntry {
    EntityRef entity;
    Real visibility = 1;
    // will affect percepts overall visiblity
    // percept.visibility = image.opacity*image.visibility;
    Real opactity = 1;
    // can always convert to polar at any time
    // these two points define the images' location
    Vector anchor_low, anchor_high;
    // how do I describe a complete overlap?
    // perhaps sentinel values
};

inline bool completely_overlaps_source(const ImageEntry & image)
    { return image.anchor_low == image.anchor_high; }

class AngleGetters final : public SpatialMapElementGetters<ImageEntry> {
public:
    Real get_low(const ImageEntry & entry) const final;

    Real get_high(const ImageEntry & entry) const final;

    Real domain_min() const final;

    Real domain_max() const final;
};

// this is a bit more of an interesting challenge, because objects may wrap
// around from k_pi*2 - e to 0
class ImageSpatialMapFactory final : public SpatialMapFactory<ImageEntry> {
public:
    MapBase & choose_map_for(SetElIterator beg, SetElIterator end, int depth) final;

private:
    using FlatMap = FlatSpatialMap<ImageEntry>;
    using PartMap = PartitionedSpatialMap<ImageEntry, AngleGetters, ImageSpatialMapFactory>;

    static constexpr const int k_fork_thershold =   16;
    static constexpr const int k_depth_max      = 1024;

    static bool too_many_shared(const SpatialMapCounts & counts)
        { return counts.shared > sum_counts(counts)*2 / 3; }

    FlatMap m_flat;
    std::unique_ptr<PartMap> m_part_ptr;
};

using RadialSpatialMap = SpatialMap<ImageEntry, ImageSpatialMapFactory>;

inline bool are_same(const PolarVector & lhs, const PolarVector & rhs)
    { return lhs.r == rhs.r && lhs.theta == rhs.theta; }

inline bool operator == (const PolarVector & lhs, const PolarVector & rhs)
    { return are_same(lhs, rhs); }

inline bool operator != (const PolarVector & lhs, const PolarVector & rhs)
    { return !are_same(lhs, rhs); }

class SightingComplete final : public Sighting {
public:
    void add_entry(const Entry &) final;

    const std::vector<Percept> & run(Vector source) final;

    std::vector<std::tuple<Vector, Vector>> make_image_lines(Vector source, std::vector<std::tuple<Vector, Vector>> && = std::vector<std::tuple<Vector, Vector>>{}) const;

private:

    template <bool kt_using_spatial_map>
    void prepare_spatial_map();

    // input
    std::vector<Entry> m_preentries;

    // output
    std::vector<Percept> m_percepts;

    // try to restrain to O(log n) since this is in a O(n) loop
    Percept find_percept_of(Vector source, const ImageEntry &) const;

    // any "workspace" variables
    using ElementContainer = RadialSpatialMap::ElementContainer;
    mutable ElementContainer m_temp_cont;
    RadialSpatialMap m_spatial_map;
    std::vector<ImageEntry> m_entries;
};

class QuadraticSightingComplete final : public Sighting {
public:
    void add_entry(const Entry &) final;

    const std::vector<Percept> & run(Vector source) final;

private:
    std::vector<Entry> m_preentries;
    std::vector<Percept> m_percepts;
    std::vector<ImageEntry> m_entries;
};

// -------------- not for client use, should be zerod out in release ----------

struct SightingUnitTestFunctions {
    // I see a lot of c apis do this, no implying that it maybe even decent
    // design...
    static SightingUnitTestFunctions make_instance();

    ImageEntry (*make_image)(Vector source, const Sighting::Entry &) = nullptr;

    Real (*find_portion_overlapped)
        (const PolarVector & subbeg, const PolarVector & sublast,
         const PolarVector & objbeg, const PolarVector & objlast) = nullptr;

    // maybe testing this is a miss?
    bool (*crosses_anti_anchor_line)(const Vector & obs, const Rectangle &) = nullptr;

    void (*update_for_possible_obstruction)
        (ImageEntry & image, const ImageEntry & other) = nullptr;

    Vector (*make_anchor)() = nullptr;
};

} // end of detail namespace -> into ::tdp

} // end of tdp namespace
