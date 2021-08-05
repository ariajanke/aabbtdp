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

#define MACRO_AABBTDP_SHOW_DETAILS_HELPERS
#include "../src/detail.hpp"
#include "../src/sight-detail.hpp"

#include "../src/SpatialMap.hpp"

#include <common/TestSuite.hpp>
#include <common/Util.hpp>

#include <iostream>
#include <random>

#include <cassert>

namespace {

using EntityA = ecs::Entity<
    Velocity, Rectangle, MapLimits, Name, ColInfo, Growth, Layer, Pushable
>;
using EntityManagerA = EntityA::ManagerType;
using Rng = std::default_random_engine;
using cul::ts::TestSuite, cul::ts::test, cul::ts::set_context, cul::ts::Unit,
      cul::Grid, tdp::detail::CollisionEvent;

void do_helper_tests          (TestSuite &);
void do_collision_matrix_tests(TestSuite &);
void do_td_physics_tests      (TestSuite &);

// more testing ideas: using multiple instances and nested probes/calls

#define mark MACRO_MARK_POSITION_OF_CUL_TEST_SUITE

} // end of <anonymous> namespace

void do_sight_unit_tests(TestSuite &);
void do_spatial_map_unit_tests(TestSuite &);

int main() {
    std::cout << "Test Entity size " << EntityA::k_component_table_size / sizeof(void *)
              << ":" << EntityA::k_component_table_size % sizeof(void *)
              << " pointers with " << EntityA::k_number_of_components_inlined << " components inlined." << std::endl;

    // note: I prefer tests that mark themselves and run in the order they
    //       appear in code
    TestSuite suite;
    suite.hide_successes();

    do_helper_tests(suite);
    do_collision_matrix_tests(suite);
    do_td_physics_tests(suite);
    do_spatial_map_unit_tests(suite);
    do_sight_unit_tests(suite);

#   ifdef MACRO_BUILD_DEMO
    run_demo();
#   endif
    return 0;
}

namespace {

auto make_tdp_handler() { return tdp::TopDownPhysicsHandler::make_instance(); }

class ColSystem final : public EntityA::SystemType {
public:
    static constexpr const Real k_et_value = 1. / 60.;

    ColSystem();

    static void run_at_least_seconds
        (EntityManagerA & eman, ColSystem & colsys, double seconds)
    {
        const int steps = int(std::round(seconds / k_et_value));
        for (int i = 0; i != steps; ++i) {
            eman.process_deletion_requests();
            eman.run_system(colsys);
        }
    }

private:
    class DefaultEventHandler final : public tdp::EventHandler {
        using EntityRef = ecs::EntityRef;
        bool check_accept_collision(EntityRef, EntityRef) const final { return true; }

        void on_collision(EntityRef a, EntityRef b, bool) final;

        void on_trespass(EntityRef, EntityRef) final {}

        void finalize_entry(EntityRef eref, Rectangle rect) final;
    };

    void update(const ContainerView & view) final;

    tdp::TdpHandlerPtr m_handle = make_tdp_handler();
};

template <typename Iter>
CollisionEvent random_col_event(Iter ent_beg, Iter ent_end, Rng & rng);

bool are_very_close(const Rectangle &, const Rectangle &);
bool are_very_close(const Vector &, const Vector &);

// ----------------------------------------------------------------------------

void do_helper_tests(TestSuite & suite) {
    suite.start_series("detail::CollisionEvent");
    mark(suite).test([] {
        CollisionEvent a, b;
        return test(   a == b && !CollisionEvent::is_less_than(a, b)
                    && !CollisionEvent::is_less_than(b, a));
    });
    mark(suite).test([] {
        EntityManagerA eman;
        auto a = eman.make_entity();
        auto b = eman.make_entity();
        auto c = eman.make_entity();
        bool c_ahead_b = c.hash() > b.hash();
        CollisionEvent a_b(a, b, true);
        CollisionEvent a_c(a, c, true);
        bool ab_lt_ac = CollisionEvent::is_less_than(a_b, a_c);
        if (c_ahead_b) {
            return test(ab_lt_ac);
        }
        return test(!ab_lt_ac);
    });
    mark(suite).test([] {
        EntityManagerA eman;
        auto a = eman.make_entity();
        auto b = eman.make_entity();
        CollisionEvent a_b(a, b, true);
        return test(a_b.first().hash() > a_b.second().hash());
    });

    set_context(suite, [](TestSuite & suite, Unit & unit) {
        EntityManagerA eman;
        std::array entities = {
            eman.make_entity(),
            eman.make_entity(),
            eman.make_entity()
        };
        std::sort(entities.begin(), entities.end(),
            [](const EntityA & a, const EntityA & b) { return a.hash() < b.hash(); });
        // order is certain now!
        auto a = entities[0];
        auto b = entities[1];
        auto c = entities[2];
        CollisionEvent a_b(a, b, true);
        CollisionEvent a_c(a, c, true);
        CollisionEvent b_c(b, c, true);
        unit.start(mark(suite), [&] {
            bool ab_ac = CollisionEvent::is_less_than(a_b, a_c);
            bool ac_bc = CollisionEvent::is_less_than(a_c, b_c);
            return test(ab_ac && ac_bc);
        });
        unit.start(mark(suite), [&] {
            return test(a_b >= a_b && b_c >= a_b);
        });
        unit.start(mark(suite), [&] {
            return test(!(a_b > a_b) && b_c > a_b);
        });
        unit.start(mark(suite), [&] {
            return test(a_b == a_b && !(b_c == a_b));
        });
        unit.start(mark(suite), [&] {
            return test(!(a_b != a_b) && b_c != a_b);
        });
        unit.start(mark(suite), [&] {
            return test(a_b <= a_b && a_c <= b_c);
        });
    });
    // mmm? is this testing weak strict ordering?
    mark(suite).test([] {
        Rng rng { 0x912EA01 };
        EntityManagerA eman;
        std::array entities = {
            eman.make_entity(), eman.make_entity(), eman.make_entity()
        };
        std::vector<CollisionEvent> col;
        for (int i = 0; i != 1024; ++i) {
            col.clear();
            for (int j = 0; j != 4; ++j) {
                col.emplace_back(random_col_event(entities.begin(), entities.end(), rng));
            }
            std::sort(col.begin(), col.end(), CollisionEvent::is_less_than);
            if (!std::is_sorted(col.begin(), col.end(), CollisionEvent::is_less_than)) {
                auto idx = std::is_sorted_until(col.begin(), col.end(), CollisionEvent::is_less_than) - col.begin();
                std::cout << std::boolalpha;
                std::cout << col[idx - 1].first().hash() << " " << col[idx - 1].second().hash() << " " << col[idx - 1].is_pushed() << std::endl;
                std::cout << col[idx].first().hash() << " " << col[idx].second().hash() << " " << col[idx].is_pushed() << std::endl;
                return test(false);
            }
        }
        return test(true);
    });
    suite.start_series("detail::find_min_push_displacement");
    // test small for push, resultant rectangle must not overlap the new
    // rectangle
    // test large push, results are non-overlapping both must move at least
    // n pixels
    // test push with large y value
    set_context(suite, [](TestSuite & suite, Unit & unit) {
        using namespace tdp::detail;
        using std::get;
        Rectangle pusher(0 , 0, 10, 10);
        Rectangle pushee(11, 0, 10, 10);
        HitSide expected_hit(k_right, k_direction_count);
        unit.start(mark(suite), [&] {
            Vector displacement(2, 0);
            auto gv = find_min_push_displacement(pusher, pushee, displacement);
            return test(   get<HitSide>(gv) == expected_hit
                        && are_very_close(get<Vector>(gv), Vector(1, 0)));
        });
        unit.start(mark(suite), [&] {
            Vector displacement(22, 0);
            auto gv = find_min_push_displacement(pusher, pushee, displacement);
            return test(   get<HitSide>(gv) == expected_hit
                        && are_very_close(get<Vector>(gv), Vector(21, 0)));
        });
        unit.start(mark(suite), [&] {
            Vector displacement(2, 8);
            auto gv = find_min_push_displacement(pusher, pushee, displacement);
            return test(   get<HitSide>(gv) == expected_hit
                        && are_very_close(get<Vector>(gv), Vector(1, 0)));
        });
    });
    // repeat going in the negative y direction
    set_context(suite, [](TestSuite & suite, Unit & unit) {
        using namespace tdp::detail;
        using std::get;
        Rectangle pusher(0,   0, 10, 10);
        Rectangle pushee(0, -11, 10, 10);
        HitSide expected_hit(k_direction_count, k_up);
        unit.start(mark(suite), [&] {
            Vector displacement(0, -2);
            auto gv = find_min_push_displacement(pusher, pushee, displacement);
            return test(   get<HitSide>(gv) == expected_hit
                        && are_very_close(get<Vector>(gv), Vector(0, -1)));
        });
        unit.start(mark(suite), [&] {
            Vector displacement(0, -22);
            auto gv = find_min_push_displacement(pusher, pushee, displacement);
            return test(   get<HitSide>(gv) == expected_hit
                        && are_very_close(get<Vector>(gv), Vector(0, -21)));
        });
        unit.start(mark(suite), [&] {
            Vector displacement(8, -2);
            auto gv = find_min_push_displacement(pusher, pushee, displacement);
            return test(   get<HitSide>(gv) == expected_hit
                        && are_very_close(get<Vector>(gv), Vector(0, -1)));
        });
    });

    // I don't think "prioritized_entries" really needs testing
    // (just calls a couple of STL iterator sequence functions)
    //
    // ah maybe one, just to see if we get the expected output...
    suite.start_series("detail::prioritized_entries");
    mark(suite).test([] {
        using tdp::detail::FullEntry;
        EntityManagerA eman;
        tdp::detail::EntryEntityRefMap map;
        auto * a = &map[eman.make_entity()];
        auto * b = &map[eman.make_entity()];
        auto * c = &map[eman.make_entity()];

        a->priority = 2;
        b->priority = 3;
        c->priority = 1;
        const std::vector<FullEntry *> cor { b, a, c };
        auto vec = tdp::detail::prioritized_entries(map, std::vector<FullEntry *>{});
        return test(std::equal(vec.begin(), vec.end(), cor.begin(), cor.end()));
    });

    suite.start_series("detail::trim_displacement_for_barriers");
    // x-ways (real y, but non crossing)
    // x-ways, non-real y
    // x-ways and y-ways
    set_context(suite, [](TestSuite & suite, Unit & unit) {
        using namespace tdp::detail;
        Rectangle trect{0, 0, 10, 10};
        Vector displacement{8, -5};
        unit.start(mark(suite), [&] {
            Vector barrier{15, -10};
            auto gv = trim_displacement_for_barriers(trect, barrier, displacement);
            return test(   gv.horizontal == k_right
                        && gv.vertical   == k_direction_count
                        && are_very_close(displacement, Vector{5, -5}));
        });
        unit.start(mark(suite), [&] {
            Vector barrier{15, k_inf};
            auto gv = trim_displacement_for_barriers(trect, barrier, displacement);
            return test(   gv.horizontal == k_right
                        && gv.vertical   == k_direction_count
                        && are_very_close(displacement, Vector{5, -5}));
        });
        unit.start(mark(suite), [&] {
            Vector barrier{15, -3};
            auto gv = trim_displacement_for_barriers(trect, barrier, displacement);
            return test(   gv.horizontal == k_right
                        && gv.vertical   == k_up
                        && are_very_close(displacement, Vector{5, -3}));
        });
    });
    mark(suite).test([] {
        using std::pow, tdp::detail::trim_displacement_for_barriers;
        for (int i = 0; true; ++i) {
            // max out at the resolution of the ones place
            // if addition here does not change the value of the floating
            // point, then I'll consider it "out of scope" of this function
            // this should break around i == 53 (on my machine anyhow)
            if (pow(2, i) + 1 == pow(2, i)) break;

            Rectangle trect{0, 0, pow(2, i), pow(2, i)};
            Vector displacement{pow(2, i) + 2, -2};
            Vector barrier     {pow(2, i) + 1, -1};
            trim_displacement_for_barriers(trect, barrier, displacement);
        }
        // passes if no assertions fire
        return test(true);
    });

#   if 0 // these aren't bad for testing the whole thing...
         // but not apporpiate for testing one function
    set_context(suite, [](TestSuite & suite, Unit & unit) {
        EntityManagerA eman;
        ColSystem colsys;
        auto pusher = eman.make_entity();
        pusher.add<Rectangle>() = Rectangle(0, 0, 10, 10);
        pusher.add<Layer>().value = layers::k_block;
        pusher.add<Name>() = "PUSHER";
        auto pushee = eman.make_entity();

        pushee.add<Rectangle>() = Rectangle(11, 0, 10, 10);
        pushee.add<Layer>().value = layers::k_block;
        pushee.add<Name>() = "PUSHEE";
        pushee.add<Pushable>();
        unit.start(mark(suite), [&] {
            // do it in pixels per frame
            pusher.add<Velocity>() = Velocity()*ColSystem::k_et_value;
            return test(false);
        });
    });
#   endif
}

void do_collision_matrix_tests(cul::ts::TestSuite & suite) {
    suite.start_series("td physics - collision matrix validity");
    // test collision matrix to make sure it's symmetric across the diagonal
    // - regular setup
    suite.test([] {
        using namespace tdp::interaction_classes;
        Grid intmatx {
            { k_as_solid  , k_as_passive, k_as_passive },
            { k_as_passive, k_as_solid  , k_as_solid   },
            { k_as_passive, k_as_solid  , k_as_solid   }
        };
        make_tdp_handler()->set_collision_matrix(std::move(intmatx));
        return test(true);
    });
    // - test catching invalid non-symmetric matrix
    suite.test([] {
        try {
            using namespace tdp::interaction_classes;
            Grid intmatx {
                { k_as_solid  , k_as_passive, k_as_passive },
                { k_as_passive, k_as_solid  , k_as_solid   },
                { k_as_passive, k_as_passive, k_as_solid   }
            };
            make_tdp_handler()->set_collision_matrix(std::move(intmatx));
        } catch (std::invalid_argument &) {
            return test(true);
        }
        return test(false);
    });
    // - using reflection sentinels
    suite.test([] {
        using namespace tdp::interaction_classes;
        Grid intmatx {
            { k_as_solid  , k_as_passive, k_as_passive },
            { k_as_passive, k_as_solid  , k_as_solid   },
            { k_reflect   , k_reflect   , k_as_solid   }
        };
        auto tdph = make_tdp_handler();
        tdph->set_collision_matrix(std::move(intmatx));
        return test(   tdph->collision_matrix()(0, 2) == k_as_passive
                    && tdph->collision_matrix()(1, 2) == k_as_solid  );
    });
    // - using reflection sentinels in an irregular, however valid fashion
    suite.test([] {
        using namespace tdp::interaction_classes;
        Grid intmatx {
            { k_as_solid  , k_reflect   , k_as_passive },
            { k_as_passive, k_as_solid  , k_reflect    },
            { k_reflect   , k_as_solid  , k_as_solid   }
        };
        auto tdph = make_tdp_handler();
        tdph->set_collision_matrix(std::move(intmatx));
        return test(   tdph->collision_matrix()(1, 0) == k_as_passive
                    && tdph->collision_matrix()(2, 1) == k_as_solid
                    && tdph->collision_matrix()(2, 0) == k_as_passive);
    });
    // - using reflection sentinels in an invalid place (the diagonal)
    suite.test([] {
        using namespace tdp::interaction_classes;
        Grid intmatx {
            { k_as_solid  , k_as_passive, k_as_passive },
            { k_as_passive, k_as_solid  , k_as_solid   },
            { k_as_passive, k_as_solid  , k_reflect    }
        };
        auto tdph = make_tdp_handler();
        try {
            tdph->set_collision_matrix(std::move(intmatx));
        } catch (std::invalid_argument &) {
            return test(true);
        }
        return test(false);
    });
    // - matrix must be a square (one test case)
    suite.test([] {
        using namespace tdp::interaction_classes;
        Grid intmatx {
            { k_as_solid  , k_as_passive, k_as_passive, k_as_trespass },
            { k_as_passive, k_as_solid  , k_as_solid  , k_as_solid    },
            { k_as_passive, k_as_solid  , k_as_solid  , k_as_solid    }
        };
        auto tdph = make_tdp_handler();
        try {
            tdph->set_collision_matrix(std::move(intmatx));
        } catch (std::invalid_argument &) {
            return test(true);
        }
        return test(false);
    });
    // - matrix both sides with reflect
    suite.test([] {
        using namespace tdp::interaction_classes;
        Grid intmatx {
            { k_as_solid  , k_as_passive, k_as_passive },
            { k_as_passive, k_as_solid  , k_reflect    },
            { k_as_passive, k_reflect   , k_as_solid   }
        };
        auto tdph = make_tdp_handler();
        try {
            tdph->set_collision_matrix(std::move(intmatx));
        } catch (std::invalid_argument &) {
            return test(true);
        }
        return test(false);
    });
    // - matrix must be set before updating an entry
    suite.test([] {
        EntityManagerA eman;
        auto e = eman.make_entity();
        e.add<Rectangle>() = Rectangle(10, 10, 10, 10);
        e.add<Layer>() = layers::k_block;
        auto uptr = tdp::TopDownPhysicsHandler::make_instance();
        bool ok = false;
        try {
            uptr->update_entry(to_tdp_entry(e, 1. / 60.));
        } catch (std::invalid_argument &) {
            ok = true;
        }
        uptr->set_collision_matrix(make_collision_matrix());
        try {
            uptr->update_entry(to_tdp_entry(e, 1. / 60.));
        } catch (std::invalid_argument &) {
            ok = false;
        }
        return test(ok);
    });
    // unit test, first time appearance trespass
    // and not more than once after the first frame
}

void do_td_physics_tests(TestSuite & suite) {
    // this is interface level stuff!
    suite.start_series("Entry");
    set_context(suite, [](TestSuite & suite, Unit & unit) {
        auto uptr = tdp::TopDownPhysicsHandler::make_instance();
        uptr->set_collision_matrix(make_collision_matrix());
        tdp::Entry entry;
        entry.bounds = Rectangle(0, 0, 10, -10);
        unit.start(mark(suite), [&] {
            try {
                uptr->update_entry(entry);
            } catch (std::invalid_argument &) {
                return test(true);
            }
            return test(false);
        });
        unit.start(mark(suite), [&] {
            entry.displacement = Vector(0, std::numeric_limits<Real>::infinity());
            entry.collision_layer = layers::k_block;
            try {
                uptr->update_entry(entry);
            } catch (std::invalid_argument &) {
                return test(true);
            }
            return test(false);
        });
        unit.start(mark(suite), [&] {
            entry.growth = Size2(10, std::numeric_limits<Real>::infinity());
            entry.collision_layer = layers::k_floor_mat;
            try {
                uptr->update_entry(entry);
            } catch (std::invalid_argument &) {
                return test(true);
            }
            return test(false);
        });
    });

    suite.start_series("synthesis - one on one entity");
    // unit tests
    // - one entity, hits a map wall
    mark(suite).test([] {
        EntityManagerA eman;
        auto e = eman.make_entity();
        e.add<Velocity >() = Velocity(2.5 / ColSystem::k_et_value, 0);
        e.add<MapLimits>() = MapLimits(0, 0, 10., 0);
        e.add<Rectangle>() = Rectangle(0., 0., 8., 8.);
        ColSystem colsys;
        eman.process_deletion_requests();
        eman.run_system(colsys);
        return test(e.get<ColInfo>().hit_wall);
    });

    // - one entity, trepasses onto another by displacement
    mark(suite).test([] {
        return test(false);
    });
    // - one entity, trepasses onto another by growth
    mark(suite).test([] {
        return test(false);
    });
    // - one entity, collides with another (not pushable)
    mark(suite).test([] {
        return test(false);
    });
    // - one entity, collides with one while another is present
    // - one entity, pushes another
    // more test ideas:
    // multi frame tests
    // multi entity tests

    suite.start_series("synthesis - growth/shrink tests");
    set_context(suite, [](TestSuite & suite, Unit & unit) {
        EntityManagerA eman;
        auto e = eman.make_entity();
        e.add<Rectangle>() = Rectangle(10, 10, 10, 10);
        e.add<Layer>() = layers::k_floor_mat;
        ColSystem colsys;
        // test growth
        unit.start(mark(suite), [&] {
            e.add<Growth>() = Size2(10, 10);
            for (int i = 0; i != int(std::ceil(1. / ColSystem::k_et_value)); ++i) {
                eman.process_deletion_requests();
                eman.run_system(colsys);
            }
            return test(are_very_close(e.get<Rectangle>(), Rectangle(5, 5, 20, 20)));
        });
        // test shrink
        unit.start(mark(suite), [&] {
            e.add<Growth>() = Size2(-5, -5);
            for (int i = 0; i != int(std::ceil(1. / ColSystem::k_et_value)); ++i) {
                eman.process_deletion_requests();
                eman.run_system(colsys);
            }
            return test(are_very_close(e.get<Rectangle>(), Rectangle(7.5, 7.5, 5, 5)));
        });
        // test shrink to zero
        unit.start(mark(suite), [&] {
            e.add<Growth>() = Size2(-15, -15);
            for (int i = 0; i != int(std::ceil(1. / ColSystem::k_et_value)); ++i) {
                eman.process_deletion_requests();
                eman.run_system(colsys);
            }
            return test(are_very_close(e.get<Rectangle>(), Rectangle(15, 15, 0, 0)));
        });
        // test growth must not be solid on any layer
        unit.start(mark(suite), [&] {
            e.add<Growth>() = Size2(10, 10);
            e.get<Layer>() = layers::k_block;
#           if 0
            // can I just use colsys?
            auto uptr = tdp::TopDownPhysicsHandler::make_instance();
            uptr->set_collision_matrix(make_collision_matrix());
#           endif
            try {
#               if 0
                uptr->update_entry(to_tdp_entry(e, 1. / 60.));
#               endif
                eman.process_deletion_requests();
                eman.run_system(colsys);
            } catch (std::invalid_argument &) {
                return test(true);
            }
            return test(false);
        });
        // make sure a growing entity hits another
        unit.start(mark(suite), [&] {
            return test(false);
        });
    });

    suite.start_series("synthesis - observed problems tests");
    // many pushers affect displacement?
    // no dice, number of pushers do not seem to affect displacement
    suite.test([] {
        EntityManagerA eman;
        static constexpr const int k_pusher_count = 200;
        for (int i = 0; i != k_pusher_count; ++i) {
            auto e = eman.make_entity();
            e.add<Rectangle>() = Rectangle{Real(i), 0, 10, 10};
            e.add<Pushable>();
        }

        auto e = eman.make_entity();
        e.add<Rectangle>() = Rectangle{10, 12, 10, 10};
        e.add<Layer>() = layers::k_block;
        e.add<Velocity>() = Velocity{10, 0};
        ColSystem colsys;
        ColSystem::run_at_least_seconds(eman, colsys, 1.);
        return test(are_very_close(cul::top_left_of(e.get<Rectangle>()),
                                   Vector{20, 12}));
    });
}

// ----------------------------------------------------------------------------

static constexpr const Real k_very_close_error = 0.005;

ColSystem::ColSystem() {
    m_handle->set_collision_matrix(make_collision_matrix());
}

void ColSystem::DefaultEventHandler::on_collision(EntityRef a, EntityRef b, bool) {
    auto ae = EntityA{ a };
    const auto & a_name = force_name(ae);
    if (b) {
        auto b_name = get_name_ptr(EntityA{ b });
        if (!b_name) return;
        ae.ensure<ColInfo>().other = b;
        ae.ensure<ColInfo>().hit_wall = false;
        std::cout << (a_name) << " has collided against " << (*b_name) << "!" << std::endl;
    } else {
        ae.ensure<ColInfo>().other = EntityRef();
        ae.ensure<ColInfo>().hit_wall = true;
        std::cout << (a_name) << " has collided against a wall!" << std::endl;
    }
}

void ColSystem::DefaultEventHandler::finalize_entry(EntityRef eref, Rectangle rect)
    { EntityA { eref }.get<Rectangle>() = rect; }

void ColSystem::update(const ContainerView & view) {
    for (auto & e : view) {
        m_handle->update_entry(to_tdp_entry(e, k_et_value));
    }
    DefaultEventHandler def_handler;
    m_handle->run(def_handler);
}

template <typename Iter>
CollisionEvent random_col_event(Iter ent_beg, Iter ent_end, Rng & rng) {
    if (ent_beg == ent_end) return tdp::detail::CollisionEvent();

    using IntDistri = std::uniform_int_distribution<int>;
    auto sel_a = ent_beg + IntDistri(0, ent_end - ent_beg - 1)(rng);
    auto sel_b = ent_beg + IntDistri(0, ent_end - ent_beg - 1)(rng);
    bool is_push = false;
    if (sel_a != sel_b) {
        is_push = IntDistri(0, 7)(rng) == 0;
    }
    return CollisionEvent(
        *sel_a,
        sel_a == sel_b ? ecs::EntityRef() : *sel_b,
        is_push);
}

bool are_very_close(const Rectangle & a, const Rectangle & b) {
    using cul::magnitude;
    //static constexpr const Real k_error = 0.005;
    return    magnitude(a.left   - b.left  ) < k_very_close_error
           && magnitude(a.top    - b.top   ) < k_very_close_error
           && magnitude(a.width  - b.width ) < k_very_close_error
           && magnitude(a.height - b.height) < k_very_close_error;
}

bool are_very_close(const Vector & a, const Vector & b) {
    return cul::magnitude(a - b) < k_very_close_error;
}

} // end of <anonymous> namespace
