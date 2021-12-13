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

#include "DemoDriver.hpp"

namespace {

constexpr const Rectangle k_field_rectangle{0, 0, k_field_width, k_field_height};
using std::make_tuple;

Entity make_player_for_any_scene(EntityManager & entity_manager);

// make rect from field center
Rectangle make_rect_from_center(Vector, Size2);

// converts a drawing function with no business with entities into a system
template <typename Func>
std::unique_ptr<GenericDrawSystem> make_as_draw_system(Func && f) {
    class Impl final : public DrawSystemWithInterface {
    public:
        Impl(Func && f): m_f(std::move(f)) {}
        void update(const ContainerView &) final {
            m_f(draw_interface());
        }
    private:
        Func m_f;
    };
    return std::make_unique<Impl>(std::move(f));
}

std::unique_ptr<GenericDrawSystem> make_composite_system
    (std::vector<std::unique_ptr<GenericDrawSystem>> && systems)
{
    class Impl final : public GenericDrawSystem {
    public:
        Impl(std::vector<std::unique_ptr<GenericDrawSystem>> && syss):
            m_syss(std::move(syss)) {}

        void update(const ContainerView & view) final {
            // yuck... have to assign and set...
            for (auto & sys_ptr : m_syss) {
                sys_ptr->update(view);
            }
        }

        void assign_interface(DrawInterface & interface) final {
            for (auto & sys_ptr : m_syss) {
                sys_ptr->assign_interface(interface);
            }
        }

        void set_player(Entity player) final {
            for (auto & sys_ptr : m_syss) {
                sys_ptr->set_player(player);
            }
        }

    private:
        std::vector<std::unique_ptr<GenericDrawSystem>> m_syss;
    };
    return std::make_unique<Impl>(std::move(systems));
}

} // end of <anonymous> namespace

SceneOptionItems
    make_from_scene_options(const SceneOptions & scene_options)
{
    std::vector<std::unique_ptr<GenericDrawSystem>> field_systems;
    std::vector<std::unique_ptr<GenericDrawSystem>> hud_systems;
    std::unique_ptr<Physics2DHandler> handler;

    switch (scene_options.sight) {
    case SceneOptions::k_no_special_sight:
        field_systems.emplace_back(std::make_unique<DrawEntitiesSystem>());
        break;
    case SceneOptions::k_sight:
        field_systems.emplace_back(std::make_unique<DrawSightSystem>());
        break;
    case SceneOptions::k_sight_with_outline:
        break;
    }
    hud_systems.emplace_back(std::make_unique<DrawHudEntitiesSystem>());

    switch (scene_options.algorithm) {
    case SceneOptions::k_quadratic:
        handler = tdp::QuadraticPhysicsHandler::make_instance();
        break;
    case SceneOptions::k_sweep: {
        auto sweep_handler = std::make_unique<tdp::IntervalSweepHandler>();
        if (scene_options.illustrated) {
            auto sys = make_as_draw_system(make_sweep_drawer(*sweep_handler, 0));

            hud_systems.emplace_back(std::move(sys));
        }
        handler = std::move(sweep_handler);
        }
        break;
    case SceneOptions::k_grid: {
        auto grid_handler = std::make_unique<tdp::GridPhysicsHandlerImpl>();
        grid_handler->reset_grid_size(k_grid_size, k_grid_size);
        if (scene_options.illustrated) {
            auto sys = make_as_draw_system(make_grid_tally_drawer(*grid_handler));
            field_systems.emplace_back(std::move(sys));
        }
        handler = std::move(grid_handler);
        }
        break;
    }



    {
    auto names_sys = std::make_unique<NameDrawSystem>();
    if (scene_options.show_push_level) {
        names_sys->assign_collision_handler(dynamic_cast<tdp::CollisionHandler &>(*handler));
    }
    field_systems.emplace_back(std::move(names_sys));
    }

    return SceneOptionItems(
        make_composite_system(std::move(hud_systems)),
        make_composite_system(std::move(field_systems)),
        std::move(handler));
}

void SceneDriver::prepare_scenes() {
    using Loader = Scene::Loader;
    // empty scene
    push_new_scene([](Loader &){});

    // of course... scene doesn't load in new systems (*can* be added though)
    // high speed projectile
    push_new_scene([](Loader & maker) {
        auto e = maker.make_entity();
        e.add<Rectangle>() = make_rect_from_center(Vector(100, 100), Size2(80, 4));
        e.add<Color>() = "#B9B";
        e.add<Name>().value = "BLOCK";
        e.add<Layer>() = layers::k_block;

        e = maker.make_entity();
        e.add<Rectangle>() = make_rect_from_center(Vector(150 - 3 / 2, 50), Size2(3, 3));
        e.add<Color>() = "#8C8";
        e.add<MapLimits>() = k_field_rectangle;
        e.add<Name>().value = "PROJ";
        e.add<Velocity>() = Vector(0, 1000);
        e.add<Layer>() = layers::k_block;

        // player is made automatically...
    });

    // many close together
    push_new_scene([](Loader & maker) {
        static constexpr const int k_pushers = 100;
        for (int i = 0; i != k_pushers; ++i) {
            auto e = maker.make_entity();
            e.add<Rectangle>() = Rectangle{Real(i % 10)*10 + 100,
                                           Real(i / 10)*10 + 100, 10, 10};
            e.add<Color, Layer, Pushable>() = make_tuple("#717", layers::k_block, Pushable{});
            e.add<Name>() = "P" + std::to_string(i);
        }
    });

    // many spread apart
    push_new_scene([](Loader & maker) {
        static constexpr const int k_pushers = 100;
        for (int i = 0; i != k_pushers; ++i) {
            auto e = maker.make_entity();

            e.add<Rectangle>() = Rectangle{Real(i % 10)*50 + 1,
                                           Real(i / 10)*50 + 1, 10, 10};
            e.add<Color, Layer, Pushable>() = make_tuple("#717", layers::k_block, Pushable{});
            e.add<Name>() = "P" + std::to_string(i);
        }
    });
#   if 0
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
            e.add<Velocity>() = Vector(-cul::normalize(r.x), 0)*Real(30.);
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
#   endif
    m_current_scene = m_scenes.end();
}

void SceneDriver::load_scene(int scene_choice, EntityManager & entity_manager, Entity & player) {
    m_scenes.at(scene_choice);
    if (m_current_scene != m_scenes.end()) {
        player = Entity{};
        (**m_current_scene).close_scene();
    }
    m_current_scene = m_scenes.begin() + scene_choice;
    (**m_current_scene).load_scene(entity_manager, player);
    if (player == Entity{}) {
        player = make_player_for_any_scene(entity_manager);
    }
}

DemoDriver::DemoDriver() {
    std::fill(m_controls.begin(), m_controls.end(), false);
}

void DemoDriver::prepare_scenes() {
    // prepare all scenes...
    m_scene_driver.prepare_scenes();

}

void DemoDriver::on_press(Key k) {
    auto idx = to_key_idx(k);
    if (idx != k_direction_count) {
        m_controls[idx] = true;
    }
}

void DemoDriver::on_release(Key k) {
    auto idx = to_key_idx(k);
    if (idx != k_direction_count) {
        m_controls[idx] = false;
    } else if (k == Key::pause) {
        m_paused = !m_paused;
    }
}

void DemoDriver::on_update() {
    if (!m_paused) {
        cul::for_all_of_base<System>(m_always_present_systems, [this](System & sys) {
            m_ent_manager.run_system(sys);
        });
    }
    if (!m_paused && m_player) {
        bool left_xor_right = m_controls[k_left_idx] ^ m_controls[k_right_idx];
        bool up_xor_down    = m_controls[k_up_idx  ] ^ m_controls[k_down_idx ];
        auto & vel = m_player.get<Velocity>();
        if (left_xor_right) vel.x = m_controls[k_left_idx] ? -1 : 1;
        else                vel.x = 0;
        if (up_xor_down   ) vel.y = m_controls[k_up_idx  ] ? -1 : 1;
        else                vel.y = 0;

        if (left_xor_right || up_xor_down) {
            vel = cul::normalize(Vector(vel.x, vel.y))*k_player_speed;
        } else {
            vel = Vector();
        }
    }

    m_ent_manager.process_deletion_requests();
}

// you see... there's a redundent "draw_interface" here...
void DemoDriver::on_draw_field(DrawInterface & draw_interface) {
    draw_backround(draw_interface, center_of(m_player.get<Rectangle>()), draw_interface.draw_area());
    m_draw_field_systems->assign_interface(draw_interface);
    m_ent_manager.run_system(*m_draw_field_systems);
}

void DemoDriver::on_draw_hud(DrawInterface & draw_interface) {
    m_draw_hud_systems->assign_interface(draw_interface);
    m_ent_manager.run_system(*m_draw_hud_systems);
}

void DemoDriver::load_scene
    (const SceneOptions & scene_options, int scene_choice)
{
    // the scene options determine which additional systems are loaded...
    m_scene_driver.load_scene(scene_choice, m_ent_manager, m_player);
    m_ent_manager.process_deletion_requests();

    {
        auto gv = make_from_scene_options(scene_options);
        m_draw_hud_systems   = std::move(gv.hud_system);
        m_draw_field_systems = std::move(gv.field_system);
        m_physics_handler    = std::move(gv.physics_handler);
    }

    for (auto * ptrptr : { &m_draw_hud_systems, &m_draw_field_systems }) {
        (**ptrptr).set_player(m_player);
    }

    m_physics_handler->set_collision_matrix(make_collision_matrix());
    std::get<CollisionSystem>(m_always_present_systems).assign_handler(*m_physics_handler);
}

/* private static */ int DemoDriver::to_key_idx(Key k) {
    switch (k) {
    case Key::up   : return k_up_idx;
    case Key::down : return k_down_idx;
    case Key::left : return k_left_idx;
    case Key::right: return k_right_idx;
    default        : return k_direction_count;
    }
}

namespace {

Entity make_player_for_any_scene(EntityManager & entity_manager) {
    auto player = entity_manager.make_entity();
    player.add<Rectangle>() = make_rect_from_center(Vector(), Size2(48, 48));
    player.add<Color>() = "#F56";
    player.add<Layer>() = layers::k_block;
    player.add<Velocity, Sight>();
    player.add<MapLimits>() = k_field_rectangle;
    return player;
}

Rectangle make_rect_from_center(Vector r, Size2 sz) {
    static constexpr const Vector k_field_center{k_field_width*0.5, k_field_height*0.5};
    return Rectangle{r + k_field_center,// - cul::convert_to<Vector>(sz)*0.5,
                     sz};
}

} // end of <anonymous> namespace
