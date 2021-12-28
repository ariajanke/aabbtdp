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

#include "components.hpp"

#include <common/Vector2Util.hpp>

using System = Entity::SystemType;

class TimeAware {
public:
    void set_elapsed_time(Real et) { m_elapsed_time = et; }

protected:
    TimeAware() {}

    Real elapsed_time() const { return m_elapsed_time; }

private:
    Real m_elapsed_time = 0;
};

class CollisionSystem final : public System, public TimeAware {
public:
    using PEntry = tdp::Entry;

    void assign_handler(Physics2DHandler & handler)
        { m_handler = &handler; }

    void update(const ContainerView & view) {
        for (auto & e : view) {
            auto entry = to_pentry(e, elapsed_time());
            if (   entry.collision_layer == tdp::Entry::k_no_layer
                && entry.entity) {
                to_pentry(e, elapsed_time());
            }
            bool skip = !entry.entity;
            if (skip && e.has_all<Rectangle, Velocity>()) {
                set_top_left_of(e.get<Rectangle>(),
                      top_left_of(e.get<Rectangle>())
                    + e.get<Velocity>().as_vector()*elapsed_time());
                continue;
            }
            if (skip) continue;
            m_handler->update_entry(entry);
        }
        m_handler->run(EventHandlerImpl::instance());
    }

    static bool is_pentry(const Entity & e)
        { return bool(to_pentry(e, 0).entity); }

private:
    class EventHandlerImpl final : public tdp::EventHandler {
    public:
        static EventHandlerImpl & instance() {
            static EventHandlerImpl inst;
            return inst;
        }

        bool check_accept_collision(EntityRef, EntityRef) const final { return true; }

        void on_collision(EntityRef a, EntityRef b, bool push_occuring) final
            { CollisionSystem::on_collision(a, b, push_occuring); }

        void on_trespass(EntityRef a, EntityRef b) final
            { CollisionSystem::on_trespass(a, b); }

        void finalize_entry(EntityRef a, Rectangle new_bounds) final
            { CollisionSystem::finalize_entry(a, new_bounds); }
    };

    static PEntry to_pentry(const Entity &, Real elapsed_time);

    static void on_collision(EntityRef a, EntityRef b, bool push_occuring);

    static void on_trespass(EntityRef a, EntityRef b);

    static void finalize_entry(EntityRef, Rectangle new_bounds);

    Physics2DHandler * m_handler = nullptr;
};

class LifetimeSystem final : public System, public TimeAware {
public:
    void update(const ContainerView & view) {
        for (auto & e : view) update(e, elapsed_time());
    }

private:
    static void update(Entity &, Real elapsed_time);
};

class SightFacingUpdateSystem final : public System {
public:
    void update(const ContainerView & view) {
        for (auto & e : view) update(e);
    }

private:
    static void update(Entity & e) {
        if (!e.has_all<Sight, Velocity>()) return;
        if (cul::magnitude(e.get<Velocity>().as_vector()) < 1) return;
        e.get<Sight>().facing = e.get<Velocity>().as_vector();
    }
};

template <typename Func>
auto make_singles_system(Func && f) {
    class Impl final : public System {
    public:
        Impl(Func && f): m_f(std::move(f)) {}

        void update(const ContainerView & view) {
            for (auto & e : view) m_f(e);
        }

    private:
        Func m_f;
    };
    return Impl(std::move(f));
}
