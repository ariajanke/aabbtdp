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

#include "demo.hpp"

#include "../src/sight-detail.hpp"
#include "../src/SpatialMap.hpp"
#include "../src/helpers.hpp"

#include "../src/physics-interval-sweep.hpp"

#include <aabbtdp/sight.hpp>
//#include "../src/PartitionBoxMap.hpp"

#include <common/sf/DrawText.hpp>
#include <common/sf/DrawRectangle.hpp>
#include <common/sf/DrawLine.hpp>
#include <common/MultiType.hpp>
#include <common/Vector2Util.hpp>

#include <SFML/Graphics/RenderTarget.hpp>
#include <SFML/Graphics/RenderWindow.hpp>
#include <SFML/Window/Event.hpp>
#include <SFML/System/Sleep.hpp>

#include <iostream>
#include <random>

#include <cassert>

#ifndef MACRO_BUILD_DEMO
#   error "Should not be included or built with a non-demo version."
#endif

namespace {

using cul::Grid;

constexpr const auto k_pc_flip_left         = sf::Keyboard::Q;
constexpr const auto k_pc_flip_right        = sf::Keyboard::E;
constexpr const auto k_pc_frame_adv_enabled = sf::Keyboard::P;
constexpr const auto k_pc_frame_adv_step    = sf::Keyboard::O;
constexpr const auto k_pc_left              = sf::Keyboard::Left;
constexpr const auto k_pc_right             = sf::Keyboard::Right;
constexpr const auto k_pc_down              = sf::Keyboard::Down;
constexpr const auto k_pc_up                = sf::Keyboard::Up;

constexpr const int k_window_height = 700;
constexpr const int k_window_width  = (k_window_height * 11) / 6;

static constexpr const Real k_field_width  = 600;
static constexpr const Real k_field_height = 600;

struct Fade {
    Real remaining = 0.;
    Real original  = 0.;
    // in intervals
    Real thershold = 2. / 3.;
};

struct Verticies : ecs::InlinedComponent {
    std::vector<sf::Vertex> verticies;
    sf::PrimitiveType primitive_type;
    const sf::Texture * texture = nullptr;
};

struct HudDrawn {};

struct ShadowImage {
    Vector velocity  = Vector(-10, -80);
    Real   fade_time = 0.5;
    Real   time_to_next_spawn  = k_inf;
    Real   time_between_spawns = k_inf;
};

struct Occupant {
    ecs::EntityRef entity;
};

struct Lifetime {
    double value;
    operator double () const { return value; }
    double & operator = (double d) { return (value = d); }
};

struct Targeting {
    struct Target {
        Target() {}
        Target(Vector l_, Real v_): loc(l_), visibility(v_) {}
        Vector loc;
        Real visibility;
    };

    Rectangle view_area;
    Vector facing;
    std::vector<Target> targets;
    std::vector<ecs::EntityRef> target_candidates;
};

struct PushLevel final {
    static constexpr const int k_default_value = 0;
    int & operator = (int i) { return (value = i); }
    int value = k_default_value;
};

using cul::DrawRectangle;
using cul::DrawText;
using RdEntity = ecs::Entity<
    DrawRectangle, Verticies, Velocity, Rectangle, MapLimits, Name, Fade,
    Pushable, HudDrawn, Layer, ShadowImage, Occupant, Lifetime, Growth,
    Bouncy, Targeting, PushLevel>;
using RdSystem = RdEntity::SystemType;
constexpr const Real k_demo_et_value = 1. / 60.;

using RealDistri = std::uniform_real_distribution<Real>;
using IntDistri  = std::uniform_int_distribution<int>;

Rectangle get_view_bounds(const sf::RenderTarget & target) {
    auto view = target.getView();
    auto size = view.getSize();
    auto cent = view.getCenter();
    return Rectangle(cul::convert_to<Vector>(cent - size*0.5f),
                     cul::convert_to<Size2 >(size));
}

void draw_grid_line(sf::RenderTarget & target, Real thickness, Real spacing) {
    assert(spacing > 0);
    assert(thickness >= 1);
    auto find_start = [](Real low, Real step) {
        auto rem = std::fmod(cul::magnitude(low), step);
        return low + ((low < 0) ? rem : -rem + step);
    };
    auto bounds = get_view_bounds(target);

    {
    int hsteps = int(std::ceil(bounds.width / spacing));
    Real x_pos = find_start(bounds.left, spacing);
    for (int i = 0; i != hsteps; ++i) {
        DrawRectangle drect(float(x_pos - thickness*0.5), float(bounds.top),
                            float(thickness), float(bounds.height));
        x_pos += spacing;
        target.draw(drect);
    }
    }
    {
    int vsteps = int(std::ceil(bounds.height / spacing));
    Real y_pos = find_start(bounds.top , spacing);
    for (int i = 0; i != vsteps; ++i) {
        DrawRectangle drect(float(bounds.left), float(y_pos - thickness*0.5),
                            float(bounds.width), float(thickness));
        y_pos += spacing;
        target.draw(drect);
    }
    }
}

class LifetimeSystem final : public RdSystem {
    void update(const ContainerView & view) final {
        for (auto & e : view) {
            if (!e.has<Lifetime>()) continue;
            auto & lifetime = e.get<Lifetime>().value;
            lifetime -= k_demo_et_value;
            if (lifetime <= 0) e.request_deletion();
        }
    }
};

class PushablePushLevelsDisplaySystem final : public RdSystem {
public:
    PushablePushLevelsDisplaySystem(sf::RenderTarget & target): m_target(target) {
        m_brush_text.load_builtin_font(cul::BitmapFont::k_8x16_highlighted_font);
    }

    void update(const ContainerView & view) final {
        for (auto & e : view) {
            if (e.has<PushLevel>() && e.has<Rectangle>()) update(e);
        }
    }

private:
    void update(const RdEntity & entity) {
        auto level = entity.get<PushLevel>().value;
        if (level == PushLevel::k_default_value) return;
        auto cent = cul::convert_to<sf::Vector2f>(cul::center_of(entity.get<Rectangle>()));
        m_brush_text.set_text_center( cent, std::to_string(level) );
        m_target.draw(m_brush_text);
    }

    DrawText m_brush_text;
    sf::RenderTarget & m_target;
};

class SpatialPartitionsDisplaySystem final : public RdSystem {
public:
    static constexpr const auto k_chosen_font = cul::BitmapFont::k_8x16_highlighted_font;
    SpatialPartitionsDisplaySystem(sf::RenderTarget & target_): target(target_) {
#       if 1
        m_max_intr_info.load_builtin_font(k_chosen_font);
#       endif
    }

private:
    void update(const ContainerView & view) final {
#       if 0
        m_entities.reserve(view.end() - view.end());
        m_inter_cont.reserve(m_entities.capacity());
        m_entities.clear();
        m_inter_cont.clear();
        for (auto & e : view) {
            if (!e.has<Rectangle>()) continue;
            m_entities.push_back(e);
        }
        for (auto & e : m_entities) {
            m_inter_cont.emplace_back(e.get<Rectangle>(), e);
        }


        m_drawables.clear();
#       endif




        static constexpr const Real    k_thickness = 2.;
        static constexpr const uint8_t k_highlight_alpha = 50;
        m_entity_rectangles.reserve(view.end() - view.begin());
        m_entity_rectangles.clear();
        for (auto & e : view) {
            if (!e.has<Rectangle>()) continue;
            m_entity_rectangles.push_back(e.get<Rectangle>());
        }
#       if 0
        m_sp_map.set_elements(m_entity_rectangles.begin(), m_entity_rectangles.end());

        Real min_x = k_inf, min_y = k_inf, max_x = -k_inf, max_y = -k_inf;
        for (const auto & rect : m_entity_rectangles) {
            min_x = std::min(min_x, rect.left);
            min_y = std::min(min_y, rect.top );
            max_x = std::max(max_x, cul::right_of (rect));
            max_y = std::max(max_y, cul::bottom_of(rect));
        }
        Rectangle parent_rect{min_x, min_y, max_x - min_x, max_y - min_y};
        m_sp_map.explore_partitions(parent_rect, [this](const Rectangle & el, int depth) {
            auto color = get_color_for_depth(depth);
            color.a = k_highlight_alpha;
            target.draw(DrawRectangle{float(el.left), float(el.top), float(el.width), float(el.height), color});
            target.draw(DrawRectangle{float(el.left), float(el.top - k_thickness / 2), float(el.width), float(k_thickness), sf::Color::White});
            target.draw(DrawRectangle{float(el.left - k_thickness / 2), float(el.top), float(k_thickness), float(el.height), sf::Color::White});

            target.draw(DrawRectangle{float(cul::right_of(el) - k_thickness / 2), float(el.top), float(k_thickness), float(el.height), sf::Color::White});
            target.draw(DrawRectangle{float(el.left), float(cul::bottom_of(el) - k_thickness / 2), float(el.width), float(k_thickness), sf::Color::White});
        });
#       endif
        {
        auto old_view = target.getView();
        auto new_view = old_view;
        new_view.setCenter(new_view.getSize()*0.5f);
        target.setView(new_view);
        target.draw(m_max_intr_info);
        target.setView(old_view);
        }
        m_max_intr_info.set_text_top_left(sf::Vector2f(0, 40),
            "Max interactions " + std::to_string(get_interaction_count())
          + " / " + std::to_string(m_entity_rectangles.size()));
    }

    static sf::Vector2f to_sfvec(Vector r)
        { return cul::convert_to<sf::Vector2f>(r); }

    static sf::Color get_color_for_depth(int depth) {
        static const std::array k_colors = {
            sf::Color::White,
            sf::Color::Red,
            sf::Color::Blue,
            sf::Color::Green,
            sf::Color::Cyan,
            sf::Color::Magenta // limit
        };
        return k_colors.at(std::min(depth, int(k_colors.size() - 1)));
    }
#   if 0
    static Rectangle adjust_for_depth(Rectangle rv, int depth) {
        return rv;
        static constexpr const Real k_step = 2;
        rv.left   += k_step*depth;
        rv.top    += k_step*depth;
        rv.width  -= k_step*depth*2;
        rv.height -= k_step*depth*2;
        return rv;
    }

    static std::array<Vector, 5> get_edge_points(const Rectangle & rect) {
        Vector tl = cul::top_left_of(rect);
        Vector tr = tl + Vector(rect.width, 0);
        Vector bl = tl + Vector(0, rect.height);
        Vector br = bl + Vector(rect.width, 0);
        return std::array { tl, tr, br, bl, tl };
    }

    using Outline = std::array<cul::DrawLine, 4>;
    struct DrawRecord {
        cul::MultiType<DrawRectangle, Outline> drawable;
        int priority = 0;
    };

    static bool compare_records(const DrawRecord & rhs, const DrawRecord & lhs) {
        return rhs.priority < lhs.priority;
    }
#   endif
    struct HorzGetters final : public tdp::detail::SpatialMapElementGetters<Rectangle> {
        Real get_low(const Rectangle & entry) const final { return entry.left; }

        Real get_high(const Rectangle & entry) const final { return cul::right_of(entry); }

        ElementCouple divide_for_exploration(const Element & el, Real division) const final {
            if (!(division >= el.left && division <= cul::right_of(el))) {
                return cut(el, el.left + el.width * 0.5);
            }
            return cut(el, division);
        }

        static ElementCouple cut(const Rectangle & el, Real division) {
            if (!(division >= el.left && division <= cul::right_of(el))) {
                throw std::runtime_error("");
            }
            auto left = el;
            left.width = division - el.left;
            auto right = el;
            right.left = cul::right_of(left);
            right.width = cul::right_of(el) - division;
            return ElementCouple{ left, right };
        }
    };

    struct VertGetters final : public tdp::detail::SpatialMapElementGetters<Rectangle> {
        Real get_low(const Rectangle & entry) const final { return entry.top; }

        Real get_high(const Rectangle & entry) const final { return cul::bottom_of(entry); }

        ElementCouple divide_for_exploration(const Element & el, Real division) const final {
            if (!(division >= el.top && division <= cul::bottom_of(el))) {
                return cut(el, el.top + el.height / 2);
            } else {
                return cut(el, division);
            }
        }

        static ElementCouple cut(const Rectangle & rect, Real division) {
            if (!(division >= rect.top && division <= cul::bottom_of(rect))) {
                throw std::runtime_error("");
            }
            auto top = rect;
            top.height = division - rect.top;
            auto bottom = rect;
            bottom.top    = cul::bottom_of(top);
            bottom.height = cul::bottom_of(rect) - division;
            return ElementCouple{ top, bottom };
        }
    };
#   if 0
    struct MapFactory final : public tdp::detail::SpatialMapFactory<Rectangle> {
        MapBase & choose_map_for(SetElIterator beg, SetElIterator end, int depth) {
            using namespace tdp::detail;
            auto choice = choose_map_for_iterators
                <Rectangle, HorzGetters, VertGetters, SetElIterator>(beg, end, depth);
            switch (choice) {
            case k_should_split_horz:
                if (!m_horz) {
                    m_horz = std::make_unique<HorzMap>();
                }
                m_horz->set_division(get_division_for<Rectangle>(beg, end, HorzGetters{}));
                return *m_horz;
            case k_should_split_vert:
                if (!m_vert) {
                    m_vert = std::make_unique<VertMap>();
                }
                m_vert->set_division(get_division_for<Rectangle>(beg, end, VertGetters{}));
                return *m_vert;
            case k_should_use_flat: return m_flat;
            }
            throw std::runtime_error("bad branch");
        }

        using HorzMap = tdp::detail::PartitionedSpatialMap<Rectangle, HorzGetters, MapFactory>;
        using VertMap = tdp::detail::PartitionedSpatialMap<Rectangle, VertGetters, MapFactory>;

        tdp::detail::FlatSpatialMap<Rectangle> m_flat;
        std::unique_ptr<HorzMap> m_horz;
        std::unique_ptr<VertMap> m_vert;

    };
#   endif
    int get_interaction_count() const {
#       if 0
        std::vector<Rectangle *> col;
        col.reserve(m_entity_rectangles.size());
        std::size_t count = 0;
        for (const auto & rect : m_entity_rectangles) {
            col = m_sp_map.collect_candidates(rect, std::move(col));
            count = std::max(col.size(), count);
        }
        return int(count);
#       endif
        return 0;
    }

#   if 0
    std::vector<RdEntity> m_entities;
    std::vector<std::tuple<Rectangle, RdEntity>> m_inter_cont;
#   endif
    std::vector<Rectangle> m_entity_rectangles;
#   if 0
    tdp::detail::SpatialMap<Rectangle, MapFactory> m_sp_map;
#   endif
    sf::RenderTarget & target;
#   if 0
    std::vector<sf::Vertex> m_verticies;
    std::vector<DrawRecord> m_drawables;
#   endif
    cul::DrawText m_max_intr_info;

};



class ShadowImageSystem final : public RdSystem {
    class EntityMaker {
    public:
        explicit EntityMaker(RdEntity source): m_source(source) {}
        RdEntity make_entity() { return m_source.make_entity(); }

    private:
        RdEntity m_source;
    };

    void update(const ContainerView & view) final {
        for (auto & e : view) {
            if (!e.has<ShadowImage>() || !e.has<DrawRectangle>()) continue;
            update(EntityMaker(e), e.get<ShadowImage>(), e.get<DrawRectangle>());
        }
    }

    static void update
        (EntityMaker emaker, ShadowImage & shad_image, const DrawRectangle & rect)
    {
        if ( (shad_image.time_to_next_spawn -= k_demo_et_value) > 0. ) return;

        shad_image.time_to_next_spawn = shad_image.time_between_spawns;

        auto shadow = emaker.make_entity();
        auto & fade = shadow.add<Fade>();
        fade.original = fade.remaining = shad_image.fade_time;
        fade.thershold = 1.;
        shadow.add<Layer>() = layers::k_passive;
        shadow.add<DrawRectangle>() = rect;
        shadow.add<Rectangle>() = Rectangle(rect.x(), rect.y(), rect.width(), rect.height());
        shadow.add<Velocity>() = shad_image.velocity;
    }
};

class RdFadeSystem final : public RdSystem {
    void update(const ContainerView & view) final {
        for (auto & e : view) {
            if (!e.has<Fade>()) continue;
            auto & fade = e.get<Fade>();
            fade.remaining -= k_demo_et_value;
            if (fade.remaining < 0.) {
                e.request_deletion();
                continue;
            }
            using U8Lims = std::numeric_limits<uint8_t>;
            auto alpha = uint8_t(std::round(U8Lims::max()
                *std::min(1., (fade.remaining / (fade.original*fade.thershold)) )
                ));
            if (auto * drect = e.ptr<DrawRectangle>()) {
                auto clr = drect->color();
                clr.a = alpha;
                drect->set_color(clr);
            }
            if (auto * vertcomp = e.ptr<Verticies>()) {
                auto clr = vertcomp->verticies.front().color;
                clr.a = alpha;
                for (auto & vtx : vertcomp->verticies) vtx.color = clr;
            }
        }
    }
};

class VerticiesMovementSystem final : public RdSystem {
    void update(const ContainerView & view) final {
        for (auto & e : view) {
            if (!e.has<Verticies>() || !e.has<Velocity>()) continue;
            auto & velocity = static_cast<Vector &>(e.get<Velocity>());
            auto displacement = cul::convert_to<sf::Vector2f>(velocity*k_demo_et_value);
            for (auto & vtx : e.get<Verticies>().verticies) {
                vtx.position += displacement;
            }
        }
    }
};
#if 0
class RdMapLimits final : public RdSystem {
public:
    static constexpr const Real k_low_x_limit  =   0;
    static constexpr const Real k_low_y_limit  =   0;
    static constexpr const Real k_high_x_limit = 600;
    static constexpr const Real k_high_y_limit = 600;

private:
    void update(const ContainerView & view) final {
        for (auto & e : view) {
            if (!e.has<Velocity>() || !e.has<MapLimits>()) continue;
            update(e.get<Velocity>(), e.get<MapLimits>());
        }
    }

    void update(const Velocity & vel, MapLimits & limits) {
        /**/ if (vel.x > 0) { limits.x = k_high_x_limit; }
        else if (vel.x < 0) { limits.x = k_low_x_limit ; }
        /**/ if (vel.y > 0) { limits.y = k_high_y_limit; }
        else if (vel.y < 0) { limits.y = k_low_y_limit ; }
    }
};
#endif
class DrawSystem final : public RdSystem {
public:
    static constexpr const auto k_chosen_font = cul::BitmapFont::k_8x8_highlighted_font;
    DrawSystem(sf::RenderTarget & target_, ecs::EntityRef m_player_ref):
        m_player_ref(m_player_ref), target(target_)
    {
        swp_ptr->set_collision_matrix(make_collision_matrix());
    }

private:
    void update(const ContainerView & view) final {
        {
        DrawRectangle drect(0, 0, k_field_width, k_field_height,
                            sf::Color(0, 70, 0));
        target.draw(drect);
        }
        draw_grid_line(target, 3., 100.);
#       if 1
        for (auto & e : view) {
            if (auto * drect = e.ptr<DrawRectangle>()) {
                if (auto * bounds = e.ptr<Rectangle>()) {
                    *drect = DrawRectangle(float(bounds->left ), float(bounds->top   ),
                                           float(bounds->width), float(bounds->height), drect->color());
                }
            }
        }
#       endif
        for (auto & e : view) {
            draw_entity(e, false);

            if (!e.has<Rectangle>()) continue;
            swp_ptr->update_entry(to_tdp_entry(e, k_demo_et_value));
        }

        auto old_view = target.getView();
        auto new_view = old_view;
        new_view.setCenter(new_view.getSize()*0.5f);
        target.setView(new_view);
        for (auto & e : view) {
            draw_entity(e, true);

        }
        target.setView(old_view);
    }

    void draw_entity(RdEntity & entity, bool draw_if_hud) {
        if (entity.has<HudDrawn>() != draw_if_hud) return;
        if (auto * drect = entity.ptr<DrawRectangle>()) {
            target.draw(*drect);
            auto * name = get_name_ptr(entity);
            if (auto * pushlevel = entity.ptr<PushLevel>()) {
                if (pushlevel->value != PushLevel::k_default_value)
                    name = nullptr;
            }
            if (name) {
                cul::DrawText dtext;
                dtext.load_builtin_font(k_chosen_font);
                dtext.set_text_center(sf::Vector2f(drect->x() + drect->width()*0.5f, drect->y() + drect->height()*0.5f), *name);
                target.draw(dtext);
            }
        }
        if (auto * vertcomp = entity.ptr<Verticies>()) {
            auto states = sf::RenderStates::Default;
            states.texture = vertcomp->texture;
            target.draw(vertcomp->verticies.data(), vertcomp->verticies.size(), vertcomp->primitive_type, states);
        }
    }

    ecs::EntityRef m_player_ref;
    std::unique_ptr<tdp::detail::IntervalSweepHandler> swp_ptr = std::make_unique<tdp::detail::IntervalSweepHandler>();
    sf::RenderTarget & target;
};

class MatFlashSystem final : public RdSystem {
public:
    static void on_trespass_try_this(RdEntity a, RdEntity b) {
        static auto handle_individual = [](RdEntity e, RdEntity other) {
            if (auto * layer = e.ptr<Layer>()) {
                if (*layer != layers::k_floor_mat) return;
            } else { return; }
            std::cout << "hello" << std::endl;
            if (e.has<ShadowImage>()) return;
            e.ensure<Occupant>().entity = other;
            auto & shad_image = e.add<ShadowImage>();
            shad_image.time_between_spawns = shad_image.time_to_next_spawn = 0.16;
        };
        handle_individual(a, b);
        handle_individual(b, a);
    }
private:
    void update(const ContainerView & view) final {
        for (auto & e : view) {
            if (!e.has<Occupant>() || !e.has<ShadowImage>()) continue;
            update(e);
        }
    }
    // copied from "detail.cpp"
    // should I reveal this from the interface?
    static bool overlaps(const Rectangle & a, const Rectangle & b) {
        return    right_of (a) > b.left && right_of (b) > a.left
               && bottom_of(a) > b.top  && bottom_of(b) > a.top ;
    }

    static void update(RdEntity & e) {
        if (e.get<Occupant>().entity) {
            const auto & other_bounds = RdEntity(e.get<Occupant>().entity).get<Rectangle>();
            if (!overlaps(e.get<Rectangle>(), other_bounds))
                e.remove<ShadowImage>();
        } else {
            e.remove<ShadowImage>();
        }
    }
};

template <typename T>
T & alt(T * ptr, T & obj)
    { return ptr ? *ptr : obj; }

template <typename T>
const T & alt(const T * ptr, const T & obj)
    { return ptr ? *ptr : obj; }

template <typename T>
const T & c_alt(const T * ptr, const T & obj)
    { return ptr ? *ptr : obj; }

void set_location_at_portion(Rectangle & rec, const Vector & por, const Vector & r) {
    if (por.x < 0 || por.x > 1 || por.y < 0 || por.y > 1) {
        throw std::out_of_range("");
    }
    rec.left = r.x - por.x*rec.width ;
    rec.top  = r.y - por.y*rec.height;
}

class TargetSystem final : public RdSystem {
public:
    TargetSystem(sf::RenderTarget & target_): target(target_) {}

    void update(const ContainerView & view) final {
        for (auto & e : view) {
            if (!e.has<Targeting>()) continue;
            update(e);
            draw(e);
        }
    }

private:
    void update(RdEntity e) {
        using SightEntry = tdp::Sighting::Entry;
        auto & tar = e.get<Targeting>();
        for (auto & eref : tar.target_candidates) {
            SightEntry entry;
            entry.entity = eref;
            entry.bounds = RdEntity{eref}.get<Rectangle>();
            m_sight->add_entry(entry);
        }
        tar.targets.clear();
        auto vision_source = cul::center_of(e.get<Rectangle>());
#       if 0
        static thread_local std::vector<std::tuple<Vector, Vector>> s_lines;
        s_lines = dynamic_cast<tdp::detail::SightingComplete &>(*m_sight).make_image_lines(vision_source, std::move(s_lines));
        static auto to_sfv2 = [] (Vector r) { return cul::convert_to<sf::Vector2f>(r); };
        for (auto & line : s_lines) {
            auto a = to_sfv2(std::get<0>(line) + vision_source);
            auto b = to_sfv2(std::get<1>(line) + vision_source);
            cul::DrawLine dline{a, b, 3.f, sf::Color::Green};
            target.draw(dline);
        }
#       endif
        for (const auto & percept : m_sight->run(vision_source)) {
            tar.targets.emplace_back(percept.target, percept.visibility);
        }
        if (auto * vel = e.ptr<Velocity>()) {
            if (vel->as_vector() != Vector{}) {
                tar.facing = cul::normalize(vel->as_vector());
            }
        }
    }

    void draw(const RdEntity & e) {
        DrawRectangle rect;
#       if 1
        static constexpr int k_size = 40;
        rect.set_size(k_size, k_size);
        for (const auto & tar : e.get<Targeting>().targets) {
            rect.set_position(tar.loc.x - k_size / 2, tar.loc.y - k_size / 2);
            rect.set_color(sf::Color(200, 200, 0, uint8_t((tar.visibility)*255)));
            target.draw(rect);
        }
#       endif
        const auto & tar = e.get<Targeting>();
        rect = DrawRectangle(float(tar.view_area.left),
                             float(tar.view_area.top),
                             float(tar.view_area.width),
                             float(tar.view_area.height),
                             sf::Color(200, 0, 100, 100));
        target.draw(rect);


    }

    std::unique_ptr<tdp::Sighting> m_sight = tdp::Sighting::make_instance();// make_quadratic_instance();
    sf::RenderTarget & target;
};

class RdColSystem final : public RdSystem {
public:
    RdColSystem() {
        m_handle->set_collision_matrix(make_collision_matrix());
    }
private:

    class DefaultEventHandler final : public tdp::EventHandler {
    public:
        void frame_reset() {
            if (m_rest_frames > 0) {
                --m_rest_frames;
            } else if (m_counter) {
                m_rest_frames = 20;
            }
            m_counter = 0;
        }
    private:
        using EntityRef = ecs::EntityRef;
        bool check_accept_collision(EntityRef, EntityRef) const final
            { return true; }

        static void check_bounce(RdEntity a, EntityRef other) {
            if (!a.has_all<Velocity, Bouncy>()) return;
            bool horz_closer = false;
            if (other) {
                if (RdEntity{other}.has<Pushable>()) return;
                horz_closer = horizontal_is_closer(a.get<Rectangle>(), RdEntity{other}.get<Rectangle>());
            } else {
                using cul::top_left_of, cul::right_of, cul::bottom_of;
                const auto & map_lims = a.get<MapLimits>().value;
                horz_closer = horizontal_is_closer(
                    a.get<Rectangle>(), top_left_of(map_lims),
                    Vector{right_of(map_lims), bottom_of(map_lims)});
            }
            auto & vel = a.get<Velocity>();
            (horz_closer ? vel.x : vel.y) *= -1;
        }

        // seems to be useful for bouncing... maybe a "useful" utility to
        // share between projects?
        static bool horizontal_is_closer(const Rectangle & a, const Rectangle & b) {
            using std::min, cul::magnitude;
            auto hdist = min(magnitude(a.left       - right_of (b)),
                             magnitude(right_of(a)  - b.left     ));
            auto vdist = min(magnitude(a.top        - bottom_of(b)),
                             magnitude(bottom_of(a) - b.top      ));
            return hdist < vdist;
        }

        static bool horizontal_is_closer
            (const Rectangle & a, const Vector & bar_a, const Vector & bar_b)
        {
            using std::min, std::max, cul::magnitude;
            Vector low_bar {min(bar_a.x, bar_b.x), min(bar_a.y, bar_b.y)};
            Vector high_bar{max(bar_a.x, bar_b.x), max(bar_a.y, bar_b.y)};
            auto hdist = min(magnitude(a.left - low_bar.x), magnitude(right_of (a) - high_bar.x));
            auto vdist = min(magnitude(a.top  - low_bar.y), magnitude(bottom_of(a) - high_bar.y));
            return hdist < vdist;
        }

        void on_collision(EntityRef a, EntityRef b, bool) final {
            //if (m_rest_frames) return;
            using cul::center_of;
            auto ae = RdEntity{a};
            check_bounce(ae, b);
            Vector spawn_point;
            std::string notice_living_place;
            const std::string * notice = nullptr;
            auto flash_text = ae.make_entity();
            //flash_text.add<HudDrawn>();
            if (b) {
                RdEntity be{b};
                check_bounce(be, a);
                static const std::string k_anon = "<ANON>";
                notice_living_place = c_alt(get_name_ptr(be), k_anon) + " & "
                                      + c_alt(get_name_ptr(ae), k_anon) + " HIT!";
                notice = &notice_living_place;

                if (flash_text.has<HudDrawn>()) {
                    spawn_point = Vector(0, m_counter*cul::SfBitmapFont::load_builtin_font(DrawSystem::k_chosen_font).character_size().height);
                    ++m_counter;
                } else {
                    spawn_point = (  center_of(ae.get<Rectangle>())
                                   + center_of(be.get<Rectangle>()))*0.5;
                }
            } else {
                static const std::string k_hit = "HIT WALL!";
                notice = &k_hit;
                spawn_point = center_of(ae.get<Rectangle>());
            }

            DrawText dtext;
            dtext.load_builtin_font(DrawSystem::k_chosen_font);
            if (flash_text.has<HudDrawn>()) {
                dtext.set_text_top_left(cul::convert_to<sf::Vector2f>(spawn_point), *notice);
            } else {
                dtext.set_text_center(cul::convert_to<sf::Vector2f>(spawn_point), *notice);
                flash_text.add<Velocity>().y = -100.;
            }

            auto & fade = flash_text.add<Fade>();
            fade.original = fade.remaining = 1.5;
            auto & vertcomp = flash_text.add<Verticies>();
            vertcomp.primitive_type = DrawText::k_primitive_type;
            vertcomp.verticies      = dtext.give_verticies();
            vertcomp.texture        = &dtext.font()->texture();
        }

        void on_trespass(EntityRef a, EntityRef b) final {
            RdEntity ae{a};
            RdEntity be{b};
            MatFlashSystem::on_trespass_try_this(ae, be);
        }

        void finalize_entry(EntityRef eref, Rectangle rect) final
            { RdEntity { eref }.get<Rectangle>() = rect; }

        int m_counter = 0;
        int m_rest_frames = 0;
    };

    void update(const ContainerView & view) final {
        for (auto & e : view) {
            if (!e.has<Rectangle>()) continue;
            auto entry = to_tdp_entry(e, k_demo_et_value);
            //entry.collision_layer = layers::k_passive;
            m_handle->update_entry(entry);
        }
        DefaultEventHandler def_handler;
        m_handle->run(def_handler);

        for (auto & e : view) {
#           if 0
            if (auto * level_comp = e.ptr<PushLevel>()) {
                if (auto * ptr = m_handle->get_push_level_for(e)) {
                    level_comp->value = *ptr;
                } else {
                    level_comp->value = PushLevel::k_default_value;
                }
            }
#           endif
            if (auto * tar = e.ptr<Targeting>()) {
                using cul::magnitude;
                if (tar->facing == Vector{}) continue;
                auto maj = cul::normalize(magnitude(tar->facing.x) > magnitude(tar->facing.y) ? Vector{tar->facing.x, 0} : Vector{0, tar->facing.y});
                Rectangle rect{Vector{}, Size2{180. + magnitude(maj.x)*200., 180. + magnitude(maj.y)*200.}};
                auto loc = cul::center_of(e.get<Rectangle>()) + maj*25.;
                if (tar->facing.x > 0) {
                    set_location_at_portion(rect, Vector{0, 0.5}, loc);
                } else if (tar->facing.x < 0) {
                    set_location_at_portion(rect, Vector{1, 0.5}, loc);
                } else if (tar->facing.y > 0) {
                    set_location_at_portion(rect, Vector{0.5, 0}, loc);
                } else {
                    set_location_at_portion(rect, Vector{0.5, 1}, loc);
                }
                tar->view_area = rect;
                tar->target_candidates.clear();
                m_handle->find_overlaps(rect, [tar, e](const tdp::Entry & entry) {
                    if (entry.entity == e) return;
                    tar->target_candidates.push_back(entry.entity);
                });
            }
        }
    }
#   if 0
    std::unique_ptr<tdp::Physics2DHandler> m_handle =
        tdp::QuadraticPhysicsHandler::make_instance();
#   else
    std::unique_ptr<tdp::detail::IntervalSweepHandler> m_handle =
        std::make_unique<tdp::detail::IntervalSweepHandler>();
#   endif
};



constexpr const auto k_left_idx        = 0;
constexpr const auto k_right_idx       = 1;
constexpr const auto k_down_idx        = 2;
constexpr const auto k_up_idx          = 3;
constexpr const auto k_direction_count = 4;
constexpr const Real k_player_speed    = 200;

template <std::size_t kt_idx, typename ... Types
          /*typename = std::enable_if_t<std::is_base_of_v<RdSystem, decltype(std::get<kt_idx>(std::tuple<Types ...>()))>, int>*/>
void run_systems_(RdEntity::ManagerType & ent_mana, std::tuple<Types ...> & tuple) {
    if constexpr (kt_idx == sizeof...(Types)) {}
    else {
        ent_mana.run_system(std::get<kt_idx>(tuple));
        run_systems_<kt_idx + 1>(ent_mana, tuple);
    }
}

template <typename ... Types>
void run_systems(RdEntity::ManagerType & ent_mana, std::tuple<Types ...> && tuple) {
    run_systems_<0>(ent_mana, tuple);
}

class EntityMaker;
// name is too general
struct FrameTimeEntityMaker {
    virtual ~FrameTimeEntityMaker() {}
    virtual void on_update(EntityMaker &&) = 0;
};

class EntityMakerBase {
public:
    RdEntity make_entity() {
        auto rv = ent_mana.make_entity();
        m_entities.push_back(rv);
        return rv;
    }

protected:
    EntityMakerBase(RdEntity::ManagerType & mana_, std::vector<RdEntity> & ents):
        ent_mana(mana_), m_entities(ents) {}

private:
    RdEntity::ManagerType & ent_mana;
    std::vector<RdEntity> & m_entities;
};

class EntityMaker final : public EntityMakerBase {
public:
    EntityMaker(RdEntity::ManagerType & mana_, std::vector<RdEntity> & ents):
        EntityMakerBase(mana_, ents) {}
};

class Scene {
public:
    void load_scene(RdEntity::ManagerType & ent_mana)
        { load_scene_(Loader(ent_mana, m_entities, m_updatable)); }

    void close_scene() {
        for (auto & e : m_entities) e.request_deletion();
        m_entities.clear();
    }

    void on_update(RdEntity::ManagerType & ent_mana) {
        if (m_updatable) m_updatable->on_update(EntityMaker(ent_mana, m_entities));
    }

    class Loader final : public EntityMakerBase {
    public:
        Loader(RdEntity::ManagerType & mana_, std::vector<RdEntity> & ents,
               std::unique_ptr<FrameTimeEntityMaker> & upda_):
            EntityMakerBase(mana_, ents), m_updatable(upda_) {}

        template <typename Func>
        void on_update(Func && f) {
            class Impl final : public FrameTimeEntityMaker {
            public:
                Impl(Func && f): m_f(std::move(f)) {}
                void on_update(EntityMaker && maker) final { m_f(maker); }
            private:
                Func m_f;
            };
            m_updatable = std::make_unique<Impl>(std::move(f));
        }

    private:
        std::unique_ptr<FrameTimeEntityMaker> & m_updatable;
    };

protected:
    virtual void load_scene_(Loader && maker) = 0;

private:
    std::vector<RdEntity> m_entities;
    std::unique_ptr<FrameTimeEntityMaker> m_updatable;
};

using SceneLoader = Scene::Loader;

template <typename Func>
std::unique_ptr<Scene> make_unique_scene(Func && f) {
    class SceneComplete final : public Scene {
    public:
        SceneComplete(Func && f): m_func(std::move(f)) {}
        void load_scene_(Loader && maker) final { m_func(maker); }
        Func m_func;
    };
    return std::make_unique<SceneComplete>(std::move(f));
}

class SceneFlipper final {
public:
    void push_scene(std::unique_ptr<Scene> ptr) {
        m_scenes.emplace_back(std::move(ptr));
    }

    void load_first_scene(RdEntity::ManagerType & mana_) {
        if (m_scenes.empty()) return;
        (**(m_pos = m_scenes.begin())).load_scene(mana_);
    }

    void navigate_left(RdEntity::ManagerType & mana_) {
        if (m_scenes.empty()) return;
        (**m_pos).close_scene();
        if (m_pos == m_scenes.begin()) {
            m_pos = m_scenes.end() - 1;
        } else {
            --m_pos;
        }
        (**m_pos).load_scene(mana_);
    }

    void navigate_right(RdEntity::ManagerType & mana_) {
        if (m_scenes.empty()) return;
        (**m_pos).close_scene();
        ++m_pos;
        if (m_pos == m_scenes.end()) {
            m_pos = m_scenes.begin();
        }
        (**m_pos).load_scene(mana_);
    }

    void on_update(RdEntity::ManagerType & ent_mana) {
        if (m_scenes.empty()) return;
        (**m_pos).on_update(ent_mana);
    }

private:
    std::vector<std::unique_ptr<Scene>> m_scenes;
    std::vector<std::unique_ptr<Scene>>::iterator m_pos;
};

Vector find_field_center() {
    static constexpr const auto k_low_x  = 0;
    static constexpr const auto k_low_y  = 0;
    static constexpr const auto k_high_x = k_field_width;
    static constexpr const auto k_high_y = k_field_height;
    return Vector(k_low_x + (k_high_x - k_low_x)*0.5,
                  k_low_y + (k_high_y - k_low_y)*0.5);
}

Rectangle make_rect_from_center(Vector r, Size2 size) {
    return Rectangle(find_field_center() + r, size);
}

RdEntity spawn_random_rectanle
    (const RdEntity & parent, EntityMakerBase & maker, const Rectangle & rect,
     Size2 min_size, Size2 max_size, std::default_random_engine & rng)
{
    auto e = maker.make_entity();
    auto w = std::round(RealDistri(min_size.width , max_size.width )(rng));
    auto h = std::round(RealDistri(min_size.height, max_size.height)(rng));
    auto x = std::round(RealDistri(rect.left      , rect.width  - w)(rng));
    auto y = std::round(RealDistri(rect.top       , rect.height - h)(rng));
    e.add<Rectangle>() = Rectangle(x, y, w, h);
    sf::Color color = sf::Color::Green;
    if (auto * drect = parent.ptr<DrawRectangle>())
        { color = drect->color(); }
    e.add<DrawRectangle>() = DrawRectangle(x, y, w, h, color);
    if (auto * vel = parent.ptr<Velocity>()) {
        e.add<Velocity>() = *vel;
    }
    if (parent.has<Pushable>()) e.add<Pushable>();
    return e;
}

void spawn_random_rectanles
    (const RdEntity & parent, EntityMakerBase & maker, const Rectangle & rect, int amount,
     Size2 min_size, Size2 max_size, std::default_random_engine & rng)
{
    for (int i = 0; i != amount; ++i) {
        (void)spawn_random_rectanle(parent, maker, rect, min_size, max_size, rng);
    }
}

class FpsCounter {
public:
    static constexpr const bool k_have_std_dev = true;
    void update(double et) {
        ++m_count_this_frame;
        push_frame_et(et);
        if ( (m_total_et += et) > 1. ) {
            m_fps = m_count_this_frame;
            m_count_this_frame = 0;
            m_total_et = std::fmod(m_total_et, 1.);
            update_second_et_std_dev();
        }
    }

    int fps() const noexcept { return m_fps; }

    std::enable_if_t<k_have_std_dev, double> std_dev() const noexcept
        { return m_et_std_dev; }

    std::enable_if_t<k_have_std_dev, double> avg() const noexcept
        { return m_et_avg;; }

private:
    void push_frame_et(double et) {
        if constexpr (k_have_std_dev) {
            m_ets.push_back(et);
        }
    }

    void update_second_et_std_dev() {
        if constexpr (k_have_std_dev) {
            m_et_avg = 0.;
            for (auto x : m_ets) m_et_avg += x;
            m_et_avg /= double(m_ets.size());

            m_et_std_dev = 0.;
            for (auto x : m_ets) {
                m_et_std_dev += (x - m_et_avg)*(x - m_et_avg);
            }
            m_et_std_dev = std::sqrt(m_et_std_dev);

            m_ets.clear();
        }
    }


    int m_fps = 0, m_count_this_frame = 0;
    double m_total_et = 0.;

    double m_et_std_dev = 0., m_et_avg = 0.;
    std::vector<double> m_ets;
};


} // end of <anonymous> namespace

void run_demo() {
    cul::SfBitmapFont::load_builtin_font(DrawSystem::k_chosen_font);
    RdEntity::ManagerType ent_mana;
    std::array<bool, k_direction_count> controls;
    std::fill(controls.begin(), controls.end(), false);
    SceneFlipper scenes;

    RdEntity player = [] (RdEntity::ManagerType & ent_mana) {
        // initialize demo world objects here
        auto player = ent_mana.make_entity();
        player.add<Velocity>();

        player.add<Rectangle>() = Rectangle(find_field_center(), Size2(50, 50));
        player.add<DrawRectangle>().set_color(sf::Color(200, 200, 255));
        player.add<MapLimits>() = Rectangle(0, 0, k_field_width, k_field_height);
        player.add<Name>().value = "PLAYER";
        player.add<Targeting>();
        return player;
    } (ent_mana);

    scenes.push_scene(make_unique_scene([](SceneLoader &){}));

    scenes.push_scene(make_unique_scene([](SceneLoader & maker) {
        static constexpr const int k_pushers = 100;
        for (int i = 0; i != k_pushers; ++i) {
            auto e = maker.make_entity();
            e.add<DrawRectangle>().set_color(sf::Color(128, 18, 128));
            e.add<Rectangle>() = Rectangle{Real(i % 10)*10 + 1,
                                           Real(i / 10)*10 + 1, 10, 10};
            e.add<Pushable>();
            e.add<Name>() = "P" + std::to_string(i);
        }
    }));
    scenes.push_scene(make_unique_scene([](SceneLoader & maker) {
        static constexpr const int k_pushers = 100;
        for (int i = 0; i != k_pushers; ++i) {
            auto e = maker.make_entity();
            e.add<DrawRectangle>().set_color(sf::Color(128, 18, 128));
            e.add<Rectangle>() = Rectangle{Real(i % 10)*50 + 1,
                                           Real(i / 10)*50 + 1, 10, 10};
            e.add<Pushable>();
            e.add<Name>() = "P" + std::to_string(i);
        }
    }));
    scenes.push_scene(make_unique_scene(
    [](SceneLoader & maker) {
        std::default_random_engine rng { 0x01239ABC };
        auto e = maker.make_entity();
        e.add<DrawRectangle>().set_color(sf::Color::Red);
        spawn_random_rectanles(e, maker, Rectangle(100, 100, 450, 150), 5,
                               Size2(20, 20), Size2(40, 40), rng);

        spawn_random_rectanles(e, maker, Rectangle(100, 400, 450, 200), 5,
                               Size2(20, 20), Size2(40, 40), rng);
        e.request_deletion();
    }
    ))
    ;

    scenes.push_scene(make_unique_scene([](SceneLoader & maker) {
        auto e = maker.make_entity();
        e.add<Rectangle>() = make_rect_from_center(Vector(0, 100), Size2 (80, 20));
        e.add<DrawRectangle>().set_color(sf::Color(200, 100, 100));
        e.add<Name>().value = "BLOCK A";

        e = maker.make_entity();
        e.add<Rectangle>() = make_rect_from_center(Vector(-100, 0), Size2(20, 80));
        e.add<DrawRectangle>().set_color(sf::Color(100, 100, 200));
        e.add<Name>().value = "BLOCK B";

        e = maker.make_entity();
        e.add<Rectangle>() = make_rect_from_center(Vector(150, -160), Size2(20, 80));
        e.add<DrawRectangle>().set_color(sf::Color(100, 200, 100));
        e.add<Name>().value = "BLOCK C";

        e = maker.make_entity();
        e.add<Rectangle>() = make_rect_from_center(Vector(150, -180), Size2(80, 20));
        e.add<DrawRectangle>().set_color(sf::Color(200, 100, 200));
        e.add<Name>().value = "BLOCK D";
    }));

    scenes.push_scene(make_unique_scene([](SceneLoader & maker) {
        auto e = maker.make_entity();
        e.add<Rectangle>() = make_rect_from_center(Vector(100, 100), Size2(80, 4));
        e.add<DrawRectangle>().set_color(sf::Color(180, 100, 180));
        e.add<Name>().value = "BLOCK";

        e = maker.make_entity();
        e.add<Rectangle>() = make_rect_from_center(Vector(150 - 3 / 2, 50), Size2(3, 3));
        e.add<DrawRectangle>().set_color(sf::Color(100, 200, 100));
        e.add<MapLimits>();
        e.add<Name>().value = "PROJ";
        e.add<Velocity>() = Vector(0, 1000);
    }));

    scenes.push_scene(make_unique_scene([](SceneLoader & maker) {
        auto e = maker.make_entity();
        e.add<Rectangle>() = make_rect_from_center(Vector(500, 400), Size2(30, 30));
        e.add<DrawRectangle>().set_color(sf::Color(180, 100, 180));
        e.add<Name>().value = "BLOCK A";
        e.add<Velocity>() = Vector(-100, -100);

        e = maker.make_entity();
        e.add<Rectangle>() = make_rect_from_center(Vector(-500, 400), Size2(30, 30));
        e.add<DrawRectangle>().set_color(sf::Color(100, 180, 180));
        e.add<Name>().value = "BLOCK B";
        e.add<Velocity>() = Vector(100, -100);
    }));

    scenes.push_scene(make_unique_scene([](SceneLoader & maker) {
        auto e = maker.make_entity();
        e.add<Rectangle>() = make_rect_from_center(Vector(-200, -200), Size2(30, 30));
        e.add<DrawRectangle>().set_color(sf::Color(100, 180, 180));
        e.add<Name>().value = "BLOCK A";
        e.add<Velocity>() = Vector(100, 0.1);

        e = maker.make_entity();
        e.add<Rectangle>() = make_rect_from_center(Vector(200, -200), Size2(30, 30));
        e.add<DrawRectangle>().set_color(sf::Color(180, 180, 100));
        e.add<Name>().value = "BLOCK B";
        e.add<Velocity>() = Vector(-100, -0.1);
    }));

    scenes.push_scene(make_unique_scene(
    [](SceneLoader & maker) {
        std::default_random_engine rng {std::random_device{}()};
        auto e = maker.make_entity();
        e.add<Bouncy>();
        e.add<Rectangle>() = make_rect_from_center(Vector(0, 250), Size2(32, 32));
        e.add<Name>().value = "BOUNCY";
        e.add<Velocity>() = cul::rotate_vector(Vector(1, 0)*RealDistri{30, 150}(rng), RealDistri{0, 3.14159265*2}(rng));
        e.add<DrawRectangle>().set_color(sf::Color(180, 180, 100));
        e.add<MapLimits>() = Rectangle(0, 0, k_field_width, k_field_height);

        e = maker.make_entity();
        e.add<Pushable>();
        e.add<Rectangle>() = make_rect_from_center(Vector(200, 0), Size2(32, 32));
        e.add<Name>().value = "BLOCK P";
        e.add<DrawRectangle>().set_color(sf::Color(180, 100, 180));
        e.add<MapLimits>() = Rectangle(0, 0, k_field_width, k_field_height);
    }
    ))
    ;
#   if 1
    // this is a stupidly complex scene
    // but an important test case still
    scenes.push_scene(make_unique_scene([](SceneLoader & maker) {
        static const Size2 k_size(30, 30);
        static const Vector k_center(-200, -200);
        auto e = maker.make_entity();
        e.add<Rectangle>() = make_rect_from_center(k_center, k_size);
        e.add<DrawRectangle>().set_color(sf::Color(180, 200, 180));
        e.add<Pushable>();

        static auto k_inner = { Vector(-1, -0.6), Vector(-1, 0.6),
                                Vector( 1, -0.6), Vector( 1, 0.6) };
        static auto k_mid = { Vector(-2, -0.6*2), Vector(-2, 0.6*2),
                              Vector( 2, -0.6*2), Vector( 2, 0.6*2) };
        static auto k_outer = { Vector(-3, -0.6*2.8), Vector(-2, 0.6*3),
                                Vector( 3, -0.6*2.8), Vector( 2, 0.6*3) };
        for (auto r : k_inner) {
            e = maker.make_entity();
            e.add<Rectangle>() = make_rect_from_center(k_center
                + Vector(r.x*k_size.width, r.y*k_size.height), k_size);
            e.add<DrawRectangle>().set_color(sf::Color(150, 150, 150));
            e.add<Pushable>();
        }

        for (auto r : k_mid) {
            e = maker.make_entity();
            e.add<Rectangle>() = make_rect_from_center(k_center
              + Vector(r.x*k_size.width, r.y*k_size.height), k_size);
            e.add<DrawRectangle>().set_color(sf::Color(90, 90, 90));
            e.add<Pushable>();
        }
        for (auto r : k_outer) {
            e = maker.make_entity();
            e.add<Rectangle>() = make_rect_from_center(k_center
                + Vector(r.x*k_size.width, r.y*k_size.height), k_size);
            e.add<DrawRectangle>().set_color(sf::Color(90, 90, 190));
            e.add<Velocity>() = Vector(-cul::normalize(r.x), 0)*30.;
        }
    }));
#   endif
    scenes.push_scene(make_unique_scene([](SceneLoader & maker) {
        static const Size2 k_size(30, 30);
        auto e = maker.make_entity();
        e.add<Rectangle>() = make_rect_from_center(Vector(0, 100), k_size);
        e.add<DrawRectangle>().set_color(sf::Color(180, 200, 180));
        e.add<Pushable>();
        e.add<MapLimits>() = Rectangle(0, 0, k_field_width, k_field_height);
        e.add<Name>().value = "PUSH A";
        e.add<PushLevel>();

        e = maker.make_entity();
        e.add<Rectangle>() = make_rect_from_center(Vector(0, 150), k_size);
        e.add<DrawRectangle>().set_color(sf::Color(180, 200, 180));
        e.add<Pushable>();
        e.add<MapLimits>() = Rectangle(0, 0, k_field_width, k_field_height);
        e.add<Name>().value = "PUSH B";
        e.add<PushLevel>();

        e = maker.make_entity();
        e.add<Rectangle>() = make_rect_from_center(Vector(0, 200), k_size);
        e.add<DrawRectangle>().set_color(sf::Color(200, 180, 180));
        e.add<Name>().value = "BLOCK";
    }));
    scenes.push_scene(make_unique_scene([](SceneLoader & maker) {
        auto e = maker.make_entity();
        e.add<DrawRectangle>().set_color(sf::Color(100, 100, 200));
        e.add<Rectangle    >() = make_rect_from_center(Vector(-100, -100), Size2(180, 80));
        e.add<Layer        >() = layers::k_floor_mat;
        //auto & shad_image = e.add<ShadowImage  >();
        //shad_image.time_between_spawns = shad_image.time_to_next_spawn = 0.16;
        e.add<Name         >().value = "FLOOR MAT";
    }));

    scenes.push_scene(make_unique_scene([](SceneLoader & loader) {
        auto rng_ptr = std::make_shared<std::default_random_engine>();
        loader.on_update([rng_ptr](EntityMaker & maker) {
            auto & rng = *rng_ptr;
            if (RealDistri(0, 1)(rng) > (1. / 100.)) return;
            auto e = maker.make_entity();
            e.add<DrawRectangle>().set_color(sf::Color::Green);
            e.request_deletion();
            int to_spawn = IntDistri(2, 5)(rng);
            for (int i = 0; i != to_spawn; ++i) {
                auto ej = spawn_random_rectanle(e, maker,
                    Rectangle(100, 100, 450, 450), Size2(5, 5), Size2(20, 20), rng);
                ej.add<Growth>() = Size2(RealDistri(-100, 100)(rng),
                                         RealDistri(-100, 100)(rng));
                ej.add<Lifetime>() = RealDistri(1, 2.5)(rng);
                ej.add<Layer>() = layers::k_floor_mat;
            }
        });
    }));

    scenes.push_scene(make_unique_scene([](SceneLoader & maker) {
        auto e = maker.make_entity();
        e.add<DrawRectangle>().set_color(sf::Color(100, 100, 200));
        e.add<Rectangle    >() = make_rect_from_center(Vector(-100, -100), Size2(180, 80));
        e.add<Layer        >() = layers::k_block;
    }));

    RdColSystem col_sys;
    sf::RenderWindow window(sf::VideoMode(k_window_width, k_window_height), " ");
    {
        auto view = window.getView();
        view.setSize(view.getSize().x / 2.f, view.getSize().y / 2.f);
        window.setView(view);
    }
    SpatialPartitionsDisplaySystem spdisplay(window);
    TargetSystem tar_sys(window);
    PushablePushLevelsDisplaySystem pplds(window);
    window.setFramerateLimit(60u);
    scenes.load_first_scene(ent_mana);
    bool in_frame_advance_mode = false;
    bool frame_advance_step    = false;
    FpsCounter fps_counter;
    sf::Clock clock;
    DrawText fps_text;
    fps_text.load_builtin_font(cul::BitmapFont::k_8x16_highlighted_font);
    while (window.isOpen()) {
        {
        sf::Event event;
        while (window.pollEvent(event)) {
            switch (event.type) {
            case sf::Event::KeyReleased:
                switch (event.key.code) {
                case k_pc_frame_adv_enabled:
                    in_frame_advance_mode = !in_frame_advance_mode;
                    break;
                case k_pc_frame_adv_step:
                    frame_advance_step = true;
                    break;
                case k_pc_flip_left :
                    scenes.navigate_left(ent_mana);
                    break;
                case k_pc_flip_right:
                    scenes.navigate_right(ent_mana);
                    break;

                default: break;
                }
                [[fallthrough]];
            case sf::Event::KeyPressed : {
                bool pressed = event.type == sf::Event::KeyPressed;
                switch (event.key.code) {
                case sf::Keyboard::Escape: window.close(); break;
                case k_pc_down : controls[k_down_idx ] = pressed; break;
                case k_pc_left : controls[k_left_idx ] = pressed; break;
                case k_pc_right: controls[k_right_idx] = pressed; break;
                case k_pc_up   : controls[k_up_idx   ] = pressed; break;
                default: break;
                }
                }
                break;
            case sf::Event::Closed:
                window.close();
                break;
            default: break;
            }
        }
        }

        {
            bool left_xor_right = controls[k_left_idx] ^ controls[k_right_idx];
            bool up_xor_down    = controls[k_up_idx  ] ^ controls[k_down_idx ];
            auto & vel = player.get<Velocity>();
            if (left_xor_right) vel.x = controls[k_left_idx] ? -1 : 1;
            else                vel.x = 0;
            if (up_xor_down   ) vel.y = controls[k_up_idx  ] ? -1 : 1;
            else                vel.y = 0;

            if (left_xor_right || up_xor_down) {
                vel = cul::normalize(Vector(vel.x, vel.y))*k_player_speed;
            } else {
                vel = Vector();
            }
        }
        {
            auto view = window.getView();
            view.setCenter(cul::convert_to<sf::Vector2f>(cul::center_of(player.get<Rectangle>())));
            window.setView(view);
        }
        if (!in_frame_advance_mode || frame_advance_step) {
            window.clear(sf::Color(80, 80, 80));
            frame_advance_step = false;
            scenes.on_update(ent_mana);
            run_systems(ent_mana, std::tuple_cat(
                std::make_tuple(DrawSystem(window, player),
                                VerticiesMovementSystem(), RdFadeSystem(),
                                ShadowImageSystem(), MatFlashSystem(),
                                LifetimeSystem()),
                std::tie(col_sys, pplds, tar_sys)));
            //run_systems(ent_mana, std::tie(spdisplay));
            ent_mana.process_deletion_requests();
        }
        [&window, &fps_counter, &fps_text, &clock] {
            auto view = window.getView();
            view.setCenter( view.getSize()*0.5f );
            window.setView(view);
            fps_counter.update(double(clock.restart().asMilliseconds()) / 1000.);
            fps_text.set_text_top_left(sf::Vector2f{0, 0}, std::to_string(fps_counter.fps()));
            window.draw(fps_text);
            window.display();
        } ();
        sf::sleep(sf::microseconds(std::int64_t(std::round(1'000'000.0 * k_demo_et_value))));

    }
}
