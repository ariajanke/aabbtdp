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

#include "defs.hpp"
#include "systems.hpp"
#include "drawing.hpp"

#include <cmath>

class FpsCounter final {
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

// ----------------------------------------------------------------------------

enum class Key {
    up, down, left, right, pause, frame_advance, none
};

// imple

constexpr const Real k_player_speed = 200;
constexpr const Real k_grid_size    = 100;

// scene loading?
// options...?
class Scene;

using EntityManager = Entity::ManagerType;

// affect which systems are present and running
struct SceneOptions final {
    enum AlgorithmOption {
        k_quadratic, k_sweep, k_grid
    };
    enum SightOption {
        k_no_special_sight, k_sight, k_sight_with_outline
    };

    AlgorithmOption algorithm = k_sweep;
    // true -> illustrate the chosen algorithm (quadratic draws nothing)
    bool illustrated = true;
    // true -> draws push level if greater than 0 (will exclude name)
    bool show_push_level = true;
    // k_no_special_sight   -> see all entities normally
    // k_sight              -> see only entities according to visibility
    // k_sight_with_outline -> see entities with visibility, but with an
    //                         outline on all
    SightOption sight = k_sight;
};

SceneOptions load_options_from_string(const std::string &);

struct SceneOptionItems final {
    SceneOptionItems() {}

    SceneOptionItems(
        std::unique_ptr<GenericDrawSystem> && hud_system_,
        std::unique_ptr<GenericDrawSystem> && field_system_,
        std::unique_ptr<Physics2DHandler>  && physics_handler_):
        hud_system(std::move(hud_system_)),
        field_system(std::move(field_system_)),
        physics_handler(std::move(physics_handler_))
    {}

    std::unique_ptr<GenericDrawSystem> hud_system;
    std::unique_ptr<GenericDrawSystem> field_system;
    std::unique_ptr<Physics2DHandler>  physics_handler;
};

SceneOptionItems make_from_scene_options(const SceneOptions &);

class EntityMakerBase {
public:
    Entity make_entity() {
        auto rv = ent_mana.make_entity();
        m_entities.push_back(rv);
        return rv;
    }

    Entity make_player();

protected:
    EntityMakerBase(EntityManager & mana_, std::vector<Entity> & ents, Entity & player):
        ent_mana(mana_), m_entities(ents), m_player(player) {}

private:
    EntityManager & ent_mana;
    std::vector<Entity> & m_entities;
    Entity & m_player;
};

class EntityMaker final : public EntityMakerBase {
public:
    EntityMaker(EntityManager & mana_, std::vector<Entity> & ents, Entity & player):
        EntityMakerBase(mana_, ents, player) {}
};

class Scene {
public:
    virtual ~Scene() {}

    void load_scene(EntityManager & ent_mana, Entity & player)
        { load_scene_(Loader(ent_mana, m_entities, player)); }

    void close_scene() {
        for (auto & e : m_entities) e.request_deletion();
        m_entities.clear();
    }

    class Loader final : public EntityMakerBase {
    public:
        Loader(EntityManager & mana_, std::vector<Entity> & ents, Entity & player):
            EntityMakerBase(mana_, ents, player) {}
    };

protected:
    virtual void load_scene_(Loader && maker) = 0;

private:
    std::vector<Entity> m_entities;
};

class SceneDriver final {
public:
    SceneDriver() {
        m_current_scene = m_scenes.end();
    }

    void prepare_scenes();

    void load_scene(int scene_choice, EntityManager &, Entity & player);

    const std::vector<std::string> & scene_names() const
        { return m_scene_names; }

private:
    template <typename Func>
    void push_new_scene(const char * name, Func && f) {
        class SceneComplete final : public Scene {
        public:
            SceneComplete(Func && f): m_func(std::move(f)) {}
            void load_scene_(Loader && maker) final { m_func(maker); }
            Func m_func;
        };
        m_scene_names.push_back(name);
        m_scenes.push_back(std::make_unique<SceneComplete>(std::move(f)));
    }

    using SceneContainer = std::vector<std::unique_ptr<Scene>>;
    SceneContainer::iterator m_current_scene;
    SceneContainer m_scenes;

    std::vector<std::string> m_scene_names;
};

class MouseState final {
public:
    void on_mouse_press(Vector field_position, const Rectangle & player_bounds);

    void on_mouse_move(Vector field_position, const Rectangle & player_bounds);

    void on_mouse_release();

    void update_player(Velocity &) const;

private:
    static constexpr const Real k_stop_thershold = 5;
    Vector m_direction;
    bool m_mouse_pressed = false;
};

class DemoDriver final {
public:
    DemoDriver();

    void prepare_scenes();

    void on_press(Key k);

    void on_release(Key k);

    void on_mouse_press(Vector field_position);

    void on_mouse_move(Vector field_position);

    void on_mouse_release();

    void on_update(Real elapsed_time);

    void on_draw_field(DrawInterface &); // const not possible?

    void on_draw_hud(DrawInterface &);

    void load_scene(const SceneOptions &, int scene_choice);

    Vector camera_center() const {
        if (m_player) return center_of(m_player.get<Rectangle>());
        return Vector{};
    }

    auto scene_names() const { return m_scene_driver.scene_names(); }

    std::string inquiry(const char * component_name) {
        static const auto k_name_map = [] {
            using std::make_pair;
            using DescribeFunc = std::string(*)(const Entity &);
            using Df = DescribeFunc;
            std::unordered_map<std::string, DescribeFunc> comp_types {
                make_pair("layer", Df([](const Entity & e) -> std::string {
                    using namespace layers;
                    if (!e.has<Layer>()) return "no layer";
                    switch (e.get<Layer>().value) {
                    case k_block      : return "block";
                    case k_floor_mat  : return "floor mat";
                    case k_passive    : return "passive";
                    default: return "<invalid layer value>";
                    }
                }))
            };
            return comp_types;
        } ();
        auto itr = k_name_map.find(component_name);
        if (itr == k_name_map.end()) {
            return std::string("Cannot find component type named \"")
                   + component_name + "\".";
        }
        std::string product;
        auto sys = make_singles_system([&product, itr](Entity & e) {
            product += force_name(e) + " " + (itr->second)(e) + "\n";
        });
        m_ent_manager.run_system(sys);
        product.pop_back();
        return product;
    }


private:
    using AlwaysPresentSystems = Tuple<
        CollisionSystem, LifetimeSystem, SightFacingUpdateSystem,
        UpdateTrespassersSystem, UpdateFloatRectangleSystem
    >;

    static int to_key_idx(Key k);

    static constexpr const auto k_left_idx        = 0;
    static constexpr const auto k_right_idx       = 1;
    static constexpr const auto k_down_idx        = 2;
    static constexpr const auto k_up_idx          = 3;
    static constexpr const auto k_direction_count = 4;
    
    std::array<bool, k_direction_count> m_controls;
    bool m_paused = false, m_frame_advancing = false;
    MouseState m_mouse_state;
    // you see, now I'm putting myself in trouble design wise...
    AlwaysPresentSystems m_always_present_systems;

    EntityManager m_ent_manager;
    Entity m_player;
    Entity m_msq;

    SceneDriver m_scene_driver;
    std::unique_ptr<Physics2DHandler> m_physics_handler;

    std::unique_ptr<GenericDrawSystem> m_draw_field_systems;
    std::unique_ptr<GenericDrawSystem> m_draw_hud_systems;
};


