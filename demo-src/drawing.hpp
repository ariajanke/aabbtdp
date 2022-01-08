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

#include <aabbtdp/sight.hpp>

// includes some private library headers...

#include "../src/CollisionHandler.hpp"
#include "../src/physics-interval-sweep.hpp"
#include "../src/physics-grid.hpp"

class DrawInterface {
public:
    virtual ~DrawInterface() {}

    virtual void draw_string_center(const std::string &, Vector center) = 0;

    virtual void draw_rectangle(const Rectangle &, const char * color) = 0;

    virtual void draw_string_top_left(const std::string &, Vector top_left) = 0;

    void draw_cone(const Vector & source, const Vector & facing, Real distance, Real spread_angle) {
        // move some checks here...
        assert(spread_angle >= 0 && spread_angle <= k_pi*2);
        assert(facing != Vector{});
        if (spread_angle < k_pi*0.005 || distance < 1) return;
        draw_cone_(source, facing, distance, spread_angle);
    }

    virtual Size2 draw_area() const = 0;

protected:
    virtual void draw_cone_(const Vector & source, const Vector & facing, Real distance, Real spread_angle) = 0;

    static Vector find_cone_point(const Vector & facing, Real distance, Real angular_pos)
        { return rotate_vector(normalize(facing)*distance, angular_pos); }
};


class GenericDrawSystem : public System {
public:
    virtual void assign_interface(DrawInterface &) = 0;

    virtual void set_player(Entity)
        { /* by default has no business with player */ }
};

class DrawSystemWithInterface : public GenericDrawSystem {
public:
    void assign_interface(DrawInterface & intf) final { m_intf = &intf; }

protected:
    DrawInterface & draw_interface() const {
        assert(m_intf);
        return *m_intf;
    }

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
};


void draw_backround(DrawInterface & draw_interface, Vector camera_center, Size2 visible_area);

// Drawing is order important... semantically systems execute whenever in
// whatever order...
//
// perhaps I can just say that they are implemented as systems (where regular
// functions would be more apporpiate)
//
// rendering is still special, OpenGL demands all operations from the main
// thread
class DrawEntitiesSystem final : public DrawSystemWithInterface {
public:
    void update(const ContainerView & view) final {
        for (auto & e : view) {
            if (!e.has<HudDrawn>()) do_individual(e);
        }
    }
};

class DrawSightSystem final : public DrawSystemWithInterface {
public:
    void set_player(Entity player) final { m_player = player; }

    void update(const ContainerView & view) final {
        auto center = center_of(m_player.get<Rectangle>());
        const auto & sight = m_player.get<Sight>();
        draw_interface().draw_cone(center, sight.facing, sight.distance, sight.spread_angle);

        for (auto & e : view) {
            if (e == m_player || !CollisionSystem::is_pentry(e)) continue;
            if (!within_sights_of(e, m_player)) continue;
            m_player_sight->add_entry(to_sight_entry(e));
        }
        for (const auto & percept : m_player_sight->run(center)) {
            auto rect = Entity{percept.entity}.get<Rectangle>();
            int alpha_val = int(std::floor(percept.visibility * 16));
            alpha_val = std::min(alpha_val, 15);
            assert(alpha_val >= 0 && alpha_val < 16);
            std::array color_string = { '#', 'F', '7', 'F', 'F', '\0' };
            color_string[4] = (alpha_val > 9) ? ((alpha_val - 10) + 'A') : (alpha_val + '0');
            draw_interface().draw_rectangle(rect, color_string.data());
        }

        do_individual(m_player);
    }

private:
    using Entry = tdp::Sighting::Entry;
    static bool within_sights_of(const Entity & e, const Entity & player) {
        auto r = center_of(e.get<Rectangle>()) - center_of(player.get<Rectangle>());
        const auto & sight = player.get<Sight>();
        static constexpr const Real k_too_close = 0.0005;
        if (   r.x*r.x + r.y*r.y > sight.distance*sight.distance
            || r.x*r.x + r.y*r.y < k_too_close*k_too_close) return false;

        return cul::angle_between(r, sight.facing) <= sight.spread_angle;
    }

    static Entry to_sight_entry(const Entity & e) {
        Entry rv;
        rv.bounds  = e.get<Rectangle>();
        rv.opacity = 1;
        rv.entity  = e;
        return rv;
    }

    Entity m_player;
    std::unique_ptr<tdp::Sighting> m_player_sight = tdp::Sighting::make_instance();
};

class DrawHudEntitiesSystem final : public DrawSystemWithInterface {
public:
    void update(const ContainerView & view) {
        for (auto & e : view) {
            if (e.has<HudDrawn>()) do_individual(e);
        }
    }
};

class NameDrawSystem final : public DrawSystemWithInterface {
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

        int i = 1;
        for (auto & e : view) {
            if (!e.has<Rectangle>()) continue;
            if (draws_plevel(e)) continue;
            if (e.has<Name>()) {
                draw_interface().draw_string_center(
                    e.get<Name>().value, center_of(e.get<Rectangle>()));
            }
#           if 0
            const auto & rect = e.get<Rectangle>();
            draw_interface().draw_string_center(
                std::to_string(rect.left) + " " + std::to_string(rect.top),
                center_of(rect) + Vector{0, 16*(i++)});
#           endif
        }

    }

private:
    CollisionHandler * m_handler = nullptr;
};

inline auto make_grid_tally_drawer(tdp::GridPhysicsHandlerImpl & handler) {
    using VecI = tdp::VectorI;
    auto handler_ptr = &handler;
    return [handler_ptr] (DrawInterface & intf) {
        auto cell_size = handler_ptr->cell_size();
        auto offset    = handler_ptr->offset();
        handler_ptr->count_each_cell([&](VecI r, int i) {
            if (i == 0) return;
            auto pt = offset + convert_to<Vector>(cell_size)*Real(0.5)
                    + Vector{cell_size.width*r.x, cell_size.height*r.y};
            intf.draw_string_center(std::to_string(i), pt);
        });
    };
}

inline auto make_sweep_drawer(tdp::IntervalSweepHandler & handler, int hud_line) {
    auto handler_ptr = &handler;
    auto str_ptr = std::make_shared<std::string>();
    return [handler_ptr, hud_line, str_ptr] (DrawInterface & intf) {
        (*str_ptr) = "Iterations for Sweep: x wise ";
        (*str_ptr) += std::to_string(handler_ptr->count_sweep_along_axis(tdp::SweepDirection::x_wise));
        (*str_ptr) += " y wise ";
        (*str_ptr) += std::to_string(handler_ptr->count_sweep_along_axis(tdp::SweepDirection::y_wise));
        intf.draw_string_top_left(*str_ptr, Vector{0, hud_line*k_hud_line_height});
    };
}
