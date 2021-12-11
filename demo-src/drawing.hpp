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

#include "systems.hpp"

// includes some private library headers...

#include "../src/CollisionHandler.hpp"
#include "../src/physics-interval-sweep.hpp"

class DrawInterface {
public:
    virtual ~DrawInterface() {}

    virtual void draw_string_center(const std::string &, Vector center) = 0;

    virtual void draw_rectangle(const Rectangle &, const char * color) = 0;

    virtual void draw_string_top_left(const std::string &, Vector top_left) = 0;

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
            draw_interface().draw_string_center(dstring->value, center_of(rect));
        }
    }


private:
    DrawInterface * m_intf = nullptr;

    Size2 m_visible_area;
    Vector m_camera_center;
};

class GenericDrawSystem : public DrawAware, public System {};

void draw_backround(DrawInterface & draw_interface, Vector camera_center, Size2 visible_area);

// Drawing is order important... semantically systems execute whenever in
// whatever order...
//
// perhaps I can just say that they are implemented as systems (where regular
// functions would be more apporpiate)
//
// rendering is still special, OpenGL demands all operations from the main
// thread
class DrawEntitiesSystem final : public System, public DrawAware {
public:
    void update(const ContainerView & view) final {
        for (auto & e : view) {
            if (!e.has<HudDrawn>()) do_individual(e);
        }
    }
};

class DrawHudEntitiesSystem final : public System, public DrawAware {
public:
    void update(const ContainerView & view) {
        for (auto & e : view) {
            if (e.has<HudDrawn>()) do_individual(e);
        }
    }
};

class NameDrawSystem final : public GenericDrawSystem {
public:
    using CollisionHandler = tdp::CollisionHandler;

    // assignment enables showing push levels
    void assign_collision_handler(CollisionHandler & handler) {
        m_handler = &handler;
    }

    void update(const ContainerView & view) final {
        // has side effects!
        auto draws_plevel = [this] (Entity & e) {
            if (!m_handler) return false;
            if (auto * plevel = m_handler->get_push_level_for(e)) {
                if (*plevel == 0) return false;
                draw_interface().draw_string_center(
                    std::to_string(*plevel), center_of(e.get<Rectangle>()));
            }
            return true;
        };
        for (auto & e : view) {
            if (!e.has<Rectangle>()) continue;
            if (draws_plevel(e)) continue;
            if (e.has<Name>()) {
                draw_interface().draw_string_center(
                    e.get<Name>().value, center_of(e.get<Rectangle>()));
            }
        }
    }

private:
    CollisionHandler * m_handler = nullptr;
};

inline auto make_sweep_drawer(tdp::IntervalSweepHandler & handler, int hud_line) {
    auto handler_ptr = &handler;
    auto str_ptr = std::make_shared<std::string>();
    return [handler_ptr, hud_line, str_ptr] (DrawInterface & intf) {
        (*str_ptr) = "x wise ";
        (*str_ptr) += std::to_string(handler_ptr->count_sweep_along_axis(tdp::SweepDirection::x_wise));
        (*str_ptr) += " y wise ";
        (*str_ptr) += std::to_string(handler_ptr->count_sweep_along_axis(tdp::SweepDirection::y_wise));
        intf.draw_string_top_left(*str_ptr, Vector{0, hud_line*k_hud_line_height});
    };
}
