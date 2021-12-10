#pragma once

#include "components.hpp"

#include <common/Vector2Util.hpp>

using System = Entity::SystemType;

class DrawInterface {
public:
    virtual ~DrawInterface() {}

    virtual void draw_string(const std::string &, Vector center) = 0;

    virtual void draw_rectangle(const Rectangle &, const char * color) = 0;

    virtual Size2 draw_area() const = 0;
};

class DrawAware {
public:

    void assign_interface(DrawInterface & intf) { m_intf = &intf; }

    // usually set once
    void set_visible_size(Size2 sz) { m_visible_area = sz; }

protected:

    DrawInterface & draw_interface() const {
        assert(m_intf);
        return *m_intf;
    }

    Size2 visible_area() const { return m_visible_area; }

    void do_individual(const Entity & e) const {
        if (!e.has<Rectangle>()) return;
        auto & rect = e.get<Rectangle>();
        if (auto * color = e.ptr<Color>()) {
            draw_interface().draw_rectangle(rect, color->string);
        }
        if (auto * dstring = e.ptr<DisplayString>()) {
            draw_interface().draw_string(dstring->value, center_of(rect));
        }
    }


private:
    DrawInterface * m_intf = nullptr;

    Size2 m_visible_area;
    Vector m_camera_center;
};

class CollisionSystem final : public System {
public:
    using PEntry = tdp::Entry;

    void assign_handler(Physics2DHandler & handler)
        { m_handler = &handler; }

    void update(const ContainerView & view) {
        for (auto & e : view) {
            auto entry = to_pentry(e);
            if (!entry.entity) continue;
            m_handler->update_entry(entry);
        }
        m_handler->run(EventHandlerImpl::instance());
    }

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

    static PEntry to_pentry(const Entity &);

    static void on_collision(EntityRef a, EntityRef b, bool push_occuring);

    static void on_trespass(EntityRef a, EntityRef b);

    static void finalize_entry(EntityRef, Rectangle new_bounds);

    Physics2DHandler * m_handler = nullptr;
};

// Drawing is order important... semantically systems execute whenever in
// whatever order...
//
// perhaps I can just say that they are implemented as systems (where regular
// functions would be more apporpiate)
//
// rendering is still special, OpenGL demands all operations from the main
// thread
class BottomLayerDrawSystem final : public System, public DrawAware {
public:

    static void draw_backround(DrawInterface & draw_interface, Vector camera_center, Size2 visible_area) {
        // "out of bounds"
        Rectangle center_rect{camera_center - convert_to<Vector>(visible_area*0.5), visible_area};
        draw_interface.draw_rectangle(center_rect, "#777");
        // "walkable" area
        draw_interface.draw_rectangle(
            Rectangle{0, 0, k_field_width, k_field_height},
            "#070");
        draw_grid_line(draw_interface, center_rect, 3, 100);
    }

    void update(const ContainerView & view) {
        for (auto & e : view) {
            if (!e.has<HudDrawn>()) do_individual(e);
        }
        for (auto & e : view) {
            if (!e.has<HudDrawn>()) {
                if (auto * name = e.ptr<Name>()) {
                    draw_interface().draw_string(name->value, center_of(e.get<Rectangle>()));
                }
            }
        }
    }

private:
    static void draw_grid_line
        (DrawInterface & draw_intf, Rectangle bounds, Real thickness, Real spacing)
    {
        assert(spacing > 0);
        assert(thickness >= 1);
        auto find_start = [](Real low, Real step) {
            auto rem = std::fmod(cul::magnitude(low), step);
            return low + ((low < 0) ? rem : -rem + step);
        };

        {
        int hsteps = int(std::ceil(bounds.width / spacing));
        Real x_pos = find_start(bounds.left, spacing);
        for (int i = 0; i != hsteps; ++i) {
            draw_intf.draw_rectangle(Rectangle{
                x_pos - thickness*0.5, bounds.top, thickness, bounds.height
            }, "#FFF");

            x_pos += spacing;
        }
        }
        {
        int vsteps = int(std::ceil(bounds.height / spacing));
        Real y_pos = find_start(bounds.top , spacing);
        for (int i = 0; i != vsteps; ++i) {
            draw_intf.draw_rectangle(Rectangle{
                bounds.left, y_pos - thickness*0.5, bounds.width, thickness
            }, "#FFF");
            y_pos += spacing;
        }
        }
    }
};

class LifetimeSystem final : public System {
public:
    void update(const ContainerView & view) {
        for (auto & e : view) update(e);
    }

private:
    static void update(Entity &);
};

class DrawHudSystem final : public System, public DrawAware {
public:
    void update(const ContainerView & view) {
        for (auto & e : view) {
            if (!e.has<HudDrawn>()) do_individual(e);
        }
    }
};
