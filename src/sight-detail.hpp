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

#include <aabbtdp/sight.hpp>

#include <common/Vector2Util.hpp>

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
    // conversion incurs a cost...
    Vector anchor_low, anchor_high;
    // how do I describe a complete overlap?
    // perhaps sentinel values
};

inline bool completely_overlaps_source(const ImageEntry & image)
    { return image.anchor_low == image.anchor_high; }

inline bool are_same(const PolarVector & lhs, const PolarVector & rhs)
    { return lhs.r == rhs.r && lhs.theta == rhs.theta; }

inline bool operator == (const PolarVector & lhs, const PolarVector & rhs)
    { return are_same(lhs, rhs); }

inline bool operator != (const PolarVector & lhs, const PolarVector & rhs)
    { return !are_same(lhs, rhs); }

// perfect for sweep interval, as it's only 1D!
class SightingComplete final : public Sighting {
public:
    using VectorPairs = std::vector<std::tuple<Vector, Vector>>;
    // if number of entries exceed this, "sweep interval" algorithm is used
    //
    // this should be performance tested to find an appropriate value
    static constexpr const int k_sweep_thershold = 12;

    void add_entry(const Entry &) final;

    const std::vector<Percept> & run(Vector source) final;

    VectorPairs make_image_lines
        (Vector source, VectorPairs && = VectorPairs{}) const;

private:
    void run_sweep_interval(Vector source);

    // input
    std::vector<Entry> m_preentries;

    // output
    std::vector<Percept> m_percepts;

    // workspace
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

    // I don't have tests yet :c
    bool (*images_overlap)(const ImageEntry &, const ImageEntry &) = nullptr;
};

} // end of detail namespace -> into ::tdp

} // end of tdp namespace
