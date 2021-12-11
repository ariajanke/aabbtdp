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

#include "systems.hpp"
#include "DemoDriver.hpp"

/* private static */ CollisionSystem::PEntry CollisionSystem::to_pentry
    (const Entity & entity)
{
    //if (!entity.has_all<Rectangle, Layer>()) return PEntry{};
    if (!entity.has<Rectangle>()) return PEntry{};
    CollisionSystem::PEntry entry;
    entry.bounds = entity.get<Rectangle>();
    if (auto * vel = entity.ptr<Velocity>())
        entry.displacement = vel->as_vector()*k_frame_time;
    if (auto * lims = entity.ptr<MapLimits>()) {
        entry.negative_barrier.x = lims->value.left;
        entry.negative_barrier.y = lims->value.top ;
        entry.positive_barrier.x = right_of (lims->value);
        entry.positive_barrier.y = bottom_of(lims->value);
    }
    if (auto * layer = entity.ptr<Layer>()) {
        entry.collision_layer = layer->value;
    } else {
        entry.collision_layer = layers::k_passive;
    }
    entry.pushable = entity.has<Pushable>();
    entry.entity = entity;
    return entry;
}

/* private static */ void CollisionSystem::on_collision
    (EntityRef a, EntityRef b, bool)
{
    static auto make_popup = [](Entity new_entity, Vector spawn_pt, std::string && str) {
        new_entity.add<Rectangle>() = Rectangle{spawn_pt - Vector(0.5, 0.5), Size2(1, 1)};
        new_entity.add<DisplayString>() = std::move(str);
        new_entity.add<Velocity>() = Vector{0, -100};
        new_entity.add<Lifetime>() = 0.8;
    };

    static auto do_for_wall = [](Entity ae) {
        make_popup(ae.make_entity(), center_of(ae.get<Rectangle>()),
                   force_name(ae) + " hit wall");
    };

    if (a && b) {
        auto pt = (  center_of(Entity{a}.get<Rectangle>())
                   + center_of(Entity{b}.get<Rectangle>())) / 2.;
        make_popup(Entity{a}.make_entity(), pt,
                   force_name(Entity{a}) + " hit " + force_name(Entity{b}));
    } else if (a) {
        do_for_wall(Entity{a});
    } else if (b) {
        do_for_wall(Entity{b});
    }
}

/* private static */ void CollisionSystem::on_trespass
    (EntityRef, EntityRef) {}

/* private static */ void CollisionSystem::finalize_entry
    (EntityRef e, Rectangle new_bounds)
{
    Entity{e}.get<Rectangle>() = new_bounds;
}


/* private static */ void LifetimeSystem::update(Entity & e) {
    if (!e.has<Lifetime>()) return;
    if ((e.get<Lifetime>().value -= k_frame_time) < 0) {
        e.request_deletion();
    }
}
