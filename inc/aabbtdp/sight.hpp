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

#include <aabbtdp/defs.hpp>

#include <memory>
#include <vector>

namespace tdp {

/// Sighting describes a set of states and their collective transformation into
/// objects called percepts.
///
/// @note on design: \n
/// Sighting can be described as a component for some entity, which is used to
/// track and create percepts for other entities around this one. As such there
/// is a persistent state associated with those percepts. \n
/// Another important design goal is that this can work well on one-frame
/// sighting events (be it associated with an entity or not).
class Sighting {
public:

    /// Creates a new instance of a sighting handler and all the underlying
    /// data structures that accompany it.
    static std::unique_ptr<Sighting> make_instance();

    /// Creates an instance which always runs in O(n^2)
    ///
    /// This function exist to ease testing
    static std::unique_ptr<Sighting> make_quadratic_instance();

    /// Each entry is a rectanglur image with a set opacity.
    struct Entry final {
        Entity    entity  = Entity{};
        Real      opacity = 1;
        Rectangle bounds;
    };

    // computations concerning whether an entity "notices" a percept, or
    // facing, or anything else is left to the client and is considered out of
    // scope for this library

    /// A percept describes an "image" of an entity, which includes its
    /// visibility and location.
    struct Percept final {
        /// The same entity reference used with the entry is passed back here
        Entity    entity = Entity{};
        /// target visibility within [0 1], where 0 is completely invisible or
        /// obstructed and 1 is opaquely completely visible
        Real      visibility = 0;
        /// target location, absolute coordinates
        Vector    target;
    };

    virtual ~Sighting() {}

    /// Adds a new AABB that maybe "seen" by this handler
    ///
    /// Other AABBs which are added between "run" calls will also be a part of
    /// sighting computations. This should not be called for an object not
    /// visible at all for any other reason.
    virtual void add_entry(const Entry &) = 0;

    /// Creates percepts for every added entry.
    ///
    /// Things which control visibility of each target/entry:
    /// - an obstructing entry's opacity will affect how much visibility is lost
    /// - a entry even if it is completely opaque may only partially obstruct
    ///   another entry, this is computed via perspective projection
    /// - if an entry's image completely covers the source then all other
    ///   entries will be obstructed to the full effect of its opacity. That is
    ///   if it's completely opaque all other entries visibility goes to zero.
    ///   If not completely opaque, then their visibility proportionally alpha
    ///   blended away.
    /// - Distance from the source will not affect visibility in any way.
    ///
    /// @note an unobstructed entry with an opacity of 1 will result in a
    ///       percept with a visibility of 1
    ///
    /// @param source The origin of observation. This is treated as the
    ///               "camera's" or "eye's" absolute location.
    /// @returns a reference to the set of percepts post process. This
    ///          container is owned by the handler and it and its members will
    ///          all become invalid if "run" is ran again.
    virtual const std::vector<Percept> & run(Vector source) = 0;
};

} // end of tdp namespace
