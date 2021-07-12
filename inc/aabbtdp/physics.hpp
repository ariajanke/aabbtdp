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

#include <ecs/ecs.hpp>

#include <common/Vector2.hpp>
#include <common/Grid.hpp>

#include <memory>

// if I want this to be a library, then a namespace is needed
namespace tdp {
    
// everything uses doubles
using Real      = double;
using Rectangle = cul::Rectangle<Real>;
using Vector    = cul::Vector2  <Real>;
using Size      = cul::Size2    <Real>;
using ecs::EntityRef;
class TopDownPhysicsHandler;
using TdpHandlerPtr = std::unique_ptr<TopDownPhysicsHandler>;

/// This structure is meant to represent a physical state related to some
/// entity.
///
/// This I cannot anticipate the desired structure/layout of any game using
/// this physics engine. This structure is defined as an intermediate between
/// this handling code and the rest of the game.
struct Entry {
    static constexpr const int k_no_layer = -1;
    static constexpr const auto k_inf = std::numeric_limits<Real>::infinity();

    /// Referenced entity for this entry, which may not be null.
    ///
    /// This value is null only for default construction of this structure.
    EntityRef entity;

    /// Defines the pre-update boundaries of the entity/entry
    ///
    /// top and left must be real numbers;
    /// width and height must be non-negative real numbers
    Rectangle bounds;

    /// Sets how much the bounds grows/shrinks this frame. Negative numbers
    /// meaning to shrink, positive for growth.
    ///
    /// This is not meant to be used on any solid entry. Doing so will result
    /// in undefined behavior. This "undefined" behavior however will not cause
    /// a crash or a generally low-level invalid program behavior. It maybe
    /// better described as "potentially undesired" whose results are not
    /// designed/implemented for in any fashion.
    ///
    /// Will cause the physics handler to throw if not real
    Size growth;

    /// Real vector which specifies how much the entry's position has changed
    /// this frame.
    ///
    /// @note This is not necessarily the actual displacement applied to the
    ///       entry, especially if the entry is pushable.
    Vector displacement;

    /// This represents a barrier specific to this entry/entity that may not be
    /// passed if computed displacement is in the negative direction.
    ///
    /// The client is free to change this value in relation to any other value.
    /// For example, changes according to position and displacement's
    /// direction.
    ///
    /// Setting it to non-real numbers will effectively remove the barrier.
    Vector negative_barrier = Vector(k_inf, k_inf);

    /// This represents a barrier specific to this entry/entity that may not be
    /// passed if computed displacement is in the positive direction.
    ///
    /// The client is free to change this value in relation to any other value.
    /// For example, changes according to position and displacement's
    /// direction.
    ///
    /// Setting it to non-real numbers will effectively remove the barrier.
    Vector positive_barrier = Vector(k_inf, k_inf);

    /// This integer specifies which collision layer this entry is identified
    /// with. It must be a number between 0 and the width/height of the
    /// collision matrix.
    int collision_layer = k_no_layer;

    /// When set to true, all interactions where this entry is solid, will
    /// cause this entry to be pushed.
    bool pushable = false;
};

/// Acts as the interface whose methods are called from the physics handler.
///
/// This class is meant to be inherited by a class/structure defined by the
/// client.
struct EventHandler {
    virtual ~EventHandler() {}

    /// Called to check if two entities should interact as solids.
    ///
    /// Parameters maybe given in any order.
    ///
    /// @note This function maybe called many, many times, so it maybe desired
    ///       to severally or restrict completely any side effects.
    ///
    /// @param a an entity reference, guaranteed to not be null
    /// @param b an entity reference, guaranteed to not be null
    /// @returns true if the two given entities should interact as solids
    virtual bool check_accept_collision(EntityRef a, EntityRef b) const = 0;

    /// Called to everytime two entities collide, or if one entity collides
    /// against a wall.
    ///
    /// @param a an entity reference, guaranteed to not be null
    /// @param b an entity reference, which is null if "a" is hitting a wall
    /// @param push_occuring true if a push is occuring, this is always false
    ///        if a wall is being hit
    virtual void on_collision(EntityRef a, EntityRef b, bool push_occuring) = 0;

    /// Called whenever two entities begin passing over each other's bounds.
    /// @param a an entity reference, guaranteed to not be null
    /// @param b an entity reference, guaranteed to not be null
    virtual void on_trespass(EntityRef a, EntityRef b) = 0;

    /// Called at the end of the physics run, this is intended to update the
    /// entity's components with the new bounds.
    ///
    /// @param new_bounds the new bounds of the entry and therefore the entity
    ///
    virtual void finalize_entry(EntityRef, Rectangle new_bounds) = 0;
};

/// This type, indicates how any two entries are to interact.
///
/// It has one sentinel value whose use should be reserved for
/// initializing/setting up a matrix.
enum class InteractionClass : uint8_t {
    /// indicates that the two objects are to be treated as solids, if
    /// "check_accept_collision" returns true (from EventHandler)
    k_as_solid,
    /// indicates that the two objects do not collide, but pass over each other
    /// and has an event
    k_as_trespass,
    /// indicates interactions which should be completely, unconditionally
    /// ignored
    k_as_passive,
    /// special value that indicates that this value should be replaced by one
    /// across the trace
    k_reflect
};

namespace interaction_classes {

constexpr const auto k_as_solid    = InteractionClass::k_as_solid;
constexpr const auto k_as_trespass = InteractionClass::k_as_trespass;
constexpr const auto k_as_passive  = InteractionClass::k_as_passive;
constexpr const auto k_reflect     = InteractionClass::k_reflect;

} // end of interaction_shorthand namespace -> into ::tdp

using CollisionMatrix = cul::Grid<InteractionClass>;

/// This represents the physics handler/updater, which describes and handles
/// all behavior for Top-Down AABB objects.
///
/// Internals are aggresively hidden, so creating an instance via a static
/// method returning a unique_ptr is needed. This should not be an issue, as
/// it's meant to be created once and used for the duration of the game.
///
/// This class is not meant to be inherited by the client, and should be used
/// only by the unique_ptr instance.
class TopDownPhysicsHandler {
public:

    /// @returns a newly created instance
    static TdpHandlerPtr make_instance();

    virtual ~TopDownPhysicsHandler() {}

    /// Sets the collision matrix for the physics handler.
    ///
    /// It must be a square matrix that is symmetric along its diagonal.
    /// "reflect" values may occupy certain elements so long as the element on
    /// the opposing side is set to a value that is not "reflect".
    ///
    /// This matrix maybe changed during this call to fit the needs of this
    /// handler.
    ///
    /// @throws if the given matrix is in anyway invalid as described above
    void set_collision_matrix(CollisionMatrix &&);

    /// @copydoc TopDownPhysicsHandler::set_collision_matrix(CollisionMatrix&&)
    void set_collision_matrix(const CollisionMatrix &);

    /// @returns a read-only reference to the set collision matrix
    /// @note this return value may not match the set value
    virtual const CollisionMatrix & collision_matrix() const = 0;

    /// Updates an entry for some entity (which is set in the given structure).
    ///
    /// Each entity has a unique entry associated with it. This function
    /// should be called every frame for every entity. This handler will
    /// delete records for entities that have expired.
    ///
    /// @throws if the entry does not have a set entity or collision layer
    virtual void update_entry(const Entry &) = 0;

    /// Runs all physics updates, accepting an event handler as a set of
    /// callbacks.
    ///
    /// This will automatically delete records for entities that have expired.
    ///
    /// @note The present implementation is currently O(n^2)
    ///       In the future this maybe reduced to O(max(n log n, m^2)), where
    ///       m is the number of mutually overlapping rectangles. So in this
    ///       hypothetical future version O(n^2) like performance would still
    ///       easily be quite possible.
    virtual void run(EventHandler &) = 0;

protected:
    virtual void set_collision_matrix_(CollisionMatrix &&) = 0;
};

} // end of tdp namespace
