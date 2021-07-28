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
#include "../src/PartitionBoxMap.hpp"
#include "../src/SpatialMap.hpp"

#include <common/TestSuite.hpp>

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
void do_sweep_and_prune_tests (TestSuite &);
void do_td_physics_tests      (TestSuite &);
void do_20210726_pmtests      (TestSuite &);

#define mark MACRO_MARK_POSITION_OF_CUL_TEST_SUITE

} // end of <anonymous> namespace

int main() {
    std::cout << "Test Entity size " << EntityA::k_component_table_size / sizeof(void *)
              << " pointers with " << EntityA::k_number_of_components_inlined << " components inlined." << std::endl;

    // note: I prefer tests that mark themselves and run in the order they
    //       appear in code
    TestSuite suite;
    suite.hide_successes();

    do_helper_tests(suite);
    do_collision_matrix_tests(suite);
    do_sweep_and_prune_tests(suite);
    do_td_physics_tests(suite);
    do_20210726_pmtests(suite);

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
        Hit expected_hit(k_right, k_direction_count);
        unit.start(mark(suite), [&] {
            Vector displacement(2, 0);
            auto gv = find_min_push_displacement(pusher, pushee, displacement);
            return test(   get<Hit>(gv) == expected_hit
                        && are_very_close(get<Vector>(gv), Vector(1, 0)));
        });
        unit.start(mark(suite), [&] {
            Vector displacement(22, 0);
            auto gv = find_min_push_displacement(pusher, pushee, displacement);
            return test(   get<Hit>(gv) == expected_hit
                        && are_very_close(get<Vector>(gv), Vector(21, 0)));
        });
        unit.start(mark(suite), [&] {
            Vector displacement(2, 8);
            auto gv = find_min_push_displacement(pusher, pushee, displacement);
            return test(   get<Hit>(gv) == expected_hit
                        && are_very_close(get<Vector>(gv), Vector(1, 0)));
        });
    });
    // repeat going in the negative y direction
    set_context(suite, [](TestSuite & suite, Unit & unit) {
        using namespace tdp::detail;
        using std::get;
        Rectangle pusher(0,   0, 10, 10);
        Rectangle pushee(0, -11, 10, 10);
        Hit expected_hit(k_direction_count, k_up);
        unit.start(mark(suite), [&] {
            Vector displacement(0, -2);
            auto gv = find_min_push_displacement(pusher, pushee, displacement);
            return test(   get<Hit>(gv) == expected_hit
                        && are_very_close(get<Vector>(gv), Vector(0, -1)));
        });
        unit.start(mark(suite), [&] {
            Vector displacement(0, -22);
            auto gv = find_min_push_displacement(pusher, pushee, displacement);
            return test(   get<Hit>(gv) == expected_hit
                        && are_very_close(get<Vector>(gv), Vector(0, -21)));
        });
        unit.start(mark(suite), [&] {
            Vector displacement(8, -2);
            auto gv = find_min_push_displacement(pusher, pushee, displacement);
            return test(   get<Hit>(gv) == expected_hit
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

void do_sweep_and_prune_tests(cul::ts::TestSuite & suite) {
    suite.start_series("sweep prune map tests");

#   if 0 // idk what to do with this test case anymore
    static constexpr const int k_fork_thershold = tdp::detail::k_pbm_partition_thershold;
    suite.test([] {
        auto w = int(std::round(std::sqrt(k_fork_thershold)));
        auto h = k_fork_thershold / w + 1;
        static constexpr const Real k_size_step = 10;
        static constexpr const Real k_block_size = 100;
        std::vector<Rectangle> rects;
        std::vector<std::tuple<Rectangle, void *>> cont;
        for (int y = 0; y != h; ++y) {
        for (int x = 0; x != w; ++x) {
            if (rects.size() == k_fork_thershold) break;
            rects.emplace_back(x*k_size_step, y*k_size_step, k_block_size, k_block_size);
        }}
        rects.emplace_back(0, (w - 1)*k_size_step, k_block_size*2, k_block_size);
        for (auto & rect : rects) {
            cont.emplace_back(rect, &rect);
        }
        auto update_last = [&cont, &rects, w](Real x)
            { rects.back().left = std::get<0>(cont.back()).left = (w - 1)*k_size_step + x; };
        using SphpMap = tdp::detail::PbHorizontalPartMap<void *>;
        Real x = 1.;
        Real last_x = 1.;
        bool cond_satis = false;
        while (true) {
            auto counts = SphpMap::get_counts(cont.begin(), cont.end());
            if (counts.high_only == 0 && counts.low_only > 0) {
                // hit gold?
                cond_satis = true;
                break;
            } else if (counts.high_only > 0) {
                break;
            }
            update_last(x);
            last_x = x;
            x *= 2.;
        }
        if (!cond_satis) { [x, last_x, &update_last, &cont] {
            Real high = x;
            Real low  = last_x;
            assert(high > low);
            while (true) {
                Real mid = low + (high - low) / 2;
                update_last(mid);
                auto counts = SphpMap::get_counts(cont.begin(), cont.end());
                assert(high != low);
                if (counts.high_only == 0 && counts.low_only > 0) {
                    // hit!
                    return;
                } else if (counts.high_only > 0) {
                    // too high
                    high = mid;
                } else {
                    // too low
                    low = mid;
                }
            }
        } (); }

        // here's the problem
        // a parent partitioned container, creates a child whoses elements
        // are the the elements which the parent received, the child and all
        // subsequent children repeat this behavior ad infinitum, therefore
        // infinite loop
        //
        // if this neither throws nor triggers a stack overflow... it passes
        tdp::detail::PartitionBoxMap<void *> spmap;
        spmap.set_elements(cont.begin(), cont.end());

        return test(true);
    });
#   endif
    // this case replicates an assertion failure
    mark(suite).test([] {
        auto rect_list = {
            Rectangle(213, 104, 37, 26), Rectangle(276, 109, 35, 37),
            Rectangle(211, 111, 37, 29), Rectangle(356, 115, 30, 34),
            Rectangle(149, 124, 35, 21),
            Rectangle(416.001, 177.712, 50, 50)
        };
        std::vector<std::tuple<Rectangle, void *>> cont;
        for (const auto & rect : rect_list) {
            cont.emplace_back(rect, nullptr);
        }
        tdp::detail::PartitionBoxMap<void *> spmap;
        spmap.set_elements(cont.begin(), cont.end());
        return test(true);
    });
    // the map must not accept rectangles with non-real numbers for any
    // attribute
    mark(suite).test([] {
        auto rect_list = {
            Rectangle(213, 104, 37, 26), Rectangle(276, 109, 35, 37),
            Rectangle(211, 111, std::numeric_limits<Real>::infinity(), 29)
        };
        std::vector<std::tuple<Rectangle, void *>> cont;
        for (const auto & rect : rect_list) {
            cont.emplace_back(rect, nullptr);
        }
        tdp::detail::PartitionBoxMap<void *> spmap;
        try {
            spmap.set_elements(cont.begin(), cont.end());
        } catch (std::invalid_argument &) {
            return test(true);
        }
        return test(false);
    });
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

// --------------------- Testing new spatial partition map --------------------

// essentially the whole namespace
using tdp::detail::sum_counts, tdp::detail::only_on_high,
      tdp::detail::only_on_low, tdp::detail::SpatialMapElementGetters,
      tdp::detail::SpatialMapFactory, tdp::detail::PartitionedSpatialMap,
      tdp::detail::get_counts, tdp::detail::FlatSpatialMap,
      tdp::detail::SpatialMapCounts, tdp::detail::SpatialMap,
      tdp::detail::pivot_sort_around, tdp::detail::pivot_sort;

struct UniDimRecord {
    UniDimRecord() {}
    UniDimRecord(Real pos_, Real len_, std::string name_ = ""):
        position(pos_),
        length  (len_),
        name    (name_)
    {}

    Real position = 0, length = 0;
    std::string name;
};

struct UniDimRecordInterface final :
    public SpatialMapElementGetters<UniDimRecord>
{
    Real get_low(const UniDimRecord & rec) const final { return rec.position; }
    Real get_high(const UniDimRecord & rec) const final { return rec.position + rec.length; }
};

constexpr const int k_test_factory_max_depth = 100;
constexpr const auto k_max_depth_exceeded_msg =
    "Maximum recursion depth for factory functions reached... this shouldn't "
    "happen in testing code.";
using RtError = std::runtime_error;

struct UniDimMapFactory final :
    public SpatialMapFactory<UniDimRecord>
{
    using PartMap = PartitionedSpatialMap<UniDimRecord, UniDimRecordInterface, UniDimMapFactory>;
    static constexpr const int k_fork_thershold = 4;
    // if shares greater than 1/2 or count < fork_thershold
    MapBase & choose_map_for(SetElIterator beg, SetElIterator end, int depth) final {
        if (depth > k_test_factory_max_depth) throw RtError(k_max_depth_exceeded_msg);
        if (end - beg < k_fork_thershold) return m_flat;
        Real avg_pt = 0;
        for (auto itr = beg; itr != end; ++itr) {
            avg_pt += (**itr).position + (**itr).length / 2;
        }
        avg_pt /= Real(end - beg);
        auto counts = get_counts<UniDimRecord, UniDimRecordInterface>(avg_pt, beg, end);
        if (counts.shared > (counts.on_high + counts.on_low + counts.shared) / 2) {
            return m_flat;
        }
        if (!m_part_map) {
            m_part_map = std::make_unique<PartMap>();
        }
        m_part_map->set_division(avg_pt);
        return *m_part_map;
    }

    FlatSpatialMap<UniDimRecord> m_flat;
    std::unique_ptr<PartMap> m_part_map;
};

struct BiDimRecord {
    BiDimRecord() {}
    BiDimRecord(Real left_, Real top_, Real wid_, Real hei_):
        bounds(left_, top_, wid_, hei_)
    {}
    Rectangle bounds;
    std::string name;
};

struct HorzRecordInterface final : public SpatialMapElementGetters<BiDimRecord> {
    Real get_low(const BiDimRecord & rec) const final
        { return rec.bounds.left; }

    Real get_high(const BiDimRecord & rec) const final
        { return cul::right_of(rec.bounds); }
};

struct VertRecordInterface final : public SpatialMapElementGetters<BiDimRecord> {
    Real get_low(const BiDimRecord & rec) const final
        { return rec.bounds.top; }

    Real get_high(const BiDimRecord & rec) const final
        { return cul::bottom_of(rec.bounds); }
};

struct BiDimMapFactory final : public SpatialMapFactory<BiDimRecord> {
    // these are essentially a forward declarations
    using HorzPartMap = PartitionedSpatialMap<BiDimRecord, HorzRecordInterface, BiDimMapFactory>;
    using VertPartMap = PartitionedSpatialMap<BiDimRecord, VertRecordInterface, BiDimMapFactory>;

    // fixed for testing purposes
    static constexpr const Real k_division = 8;

    // if shares greater than 1/2 or count < fork_thershold
    MapBase & choose_map_for(SetElIterator beg, SetElIterator end, int depth) final {
#       if 0
        std::cout << (end - beg) << " items in depth " << depth << " level(s)" << std::endl;
#       endif
        switch (depth) {
        case 0:
            if (!m_horz_map_ptr) {
                m_horz_map_ptr = std::make_unique<HorzPartMap>();
            }
            m_horz_map_ptr->set_division(k_division);
            return *m_horz_map_ptr;
        case 1: // at this depth, should be divided into quadrants
            if (!m_vert_map_ptr) {
                m_vert_map_ptr = std::make_unique<VertPartMap>();
            }
            m_vert_map_ptr->set_division(k_division);
            return *m_vert_map_ptr;
        case 2:
            return m_flat;
        default: throw RtError(k_max_depth_exceeded_msg);
        }
    }

    FlatSpatialMap<BiDimRecord> m_flat;
    std::unique_ptr<HorzPartMap> m_horz_map_ptr;
    std::unique_ptr<VertPartMap> m_vert_map_ptr;
};

// --- other helpers for testing ---

template <typename T>
std::vector<T *> convert_to_pointer_vector(std::vector<T> & vec) {
    std::vector<T *> rv;
    rv.reserve(vec.size());
    for (auto & r : vec) rv.push_back(&r);
    return rv;
}

using UniDimRecPtrConstIter = std::vector<UniDimRecord *>::const_iterator;

bool any_has_position(Real pos, UniDimRecPtrConstIter beg, UniDimRecPtrConstIter end) {
    for (auto itr = beg; itr != end; ++itr) {
        if (std::equal_to<Real>{}((**itr).position, pos)) {
            return true;
        }
    }
    return false;
}

void do_20210726_pmtests(TestSuite & suite) {
    using TestUniMap = SpatialMap<UniDimRecord, UniDimMapFactory>;
    suite.start_series("new spatial partition map");
    set_context(suite, [](TestSuite & suite, Unit & unit) {
        TestUniMap map;
        unit.start(mark(suite), [&map] {
            std::vector<UniDimRecord> col = {
                UniDimRecord { 0  , 1 },
                UniDimRecord { 0.5, 1 },
                UniDimRecord { 1  , 1 }
            };
            map.set_elements(col.begin(), col.end());
            // the partition map does not handle intersection logic
            // it only collects possible intersections
            return test(map.collect_candidates(UniDimRecord{ 0, 0.1 }).size() == 3);
        });

    });
    // lets test partial sort
    mark(suite).test([] {
        using namespace cul::exceptions_abbr;
        std::vector<int> col = { 9, 8, 3, 2, 1, 4, 10, 4 };
        auto on_low = [](int i) { return i <= 4; };
        pivot_sort(col.begin(), col.end(), on_low);
        auto itr = col.begin();
        int low_count = 0;
        for (; itr != col.end(); ++itr) {
            if (!on_low(*itr)) break;
            ++low_count;
        }
        int high_count = 0;
        for (; itr != col.end(); ++itr) {
            if (on_low(*itr)) throw RtError("partial sort failed");
            ++high_count;
        }
        return test(low_count == 5 && high_count == 3);
    });
    // two items

    //
    mark(suite).test([] {
        std::vector<UniDimRecord> col = {
            UniDimRecord { 0  , 1 },
            UniDimRecord { 1.5, 1 },
            UniDimRecord { 3.0, 1 }
        };
        return test(   !only_on_low (UniDimRecordInterface{}, col[1], 2)
                    && !only_on_high(UniDimRecordInterface{}, col[1], 2));
    });

    mark(suite).test([] {
        std::vector<UniDimRecord> col = {
            UniDimRecord { 0  , 1 },
            UniDimRecord { 1.5, 1 },
            UniDimRecord { 3.0, 1 }
        };
        auto ptrcol = convert_to_pointer_vector(col);
        auto gv = pivot_sort_around<UniDimRecord, UniDimRecordInterface>(ptrcol.begin(), ptrcol.end(), 2);
        return test(   gv.high_beg == ptrcol.begin() + 1
                    && gv.low_end  == ptrcol.begin() + 2);
    });
    // do a partial sort that actually changes the order of the elements
    // both one side (moving low only)
    mark(suite).test([] {
        std::vector col = {
            UniDimRecord { 0.5, 1 },
            UniDimRecord { 1.5, 1 },
            UniDimRecord { 2.5, 1 },
            UniDimRecord { 3.0, 1 },
            UniDimRecord { 0  , 1 } // <- this needs to be moved
        };
        auto ptrcol = convert_to_pointer_vector(col);
        auto gv = pivot_sort_around<UniDimRecord, UniDimRecordInterface>(ptrcol.begin(), ptrcol.end(), 2);
        return test(   gv.low_end - ptrcol.begin() == 3
                    && any_has_position(0, ptrcol.begin(), gv.low_end));
    });
    // and two sided (low and high)
    mark(suite).test([] {
        std::vector col = {
            UniDimRecord { 3.0, 1 }, // <- this needs to be moved
            UniDimRecord { 0.5, 1 },
            UniDimRecord { 1.5, 1 },
            UniDimRecord { 2.5, 1 },
            UniDimRecord { 0  , 1 } // <- as does this
        };
        auto ptrcol = convert_to_pointer_vector(col);
        auto gv = pivot_sort_around<UniDimRecord, UniDimRecordInterface>(ptrcol.begin(), ptrcol.end(), 2);
        return test(   any_has_position(3, gv.high_beg   , ptrcol.end())
                    && any_has_position(0, ptrcol.begin(), gv.low_end  ));

    });
    // test low only candidates
    mark(suite).test([] {
        TestUniMap map;
        std::vector<UniDimRecord> col = {
            UniDimRecord { 0  , 1 },
            UniDimRecord { 0.5, 1 },
            UniDimRecord { 2.5, 1 },
            UniDimRecord { 3.0, 1 }
        };
        map.set_elements(col.begin(), col.end());
        return test(map.collect_candidates(UniDimRecord{0.75, 0.5}).size() == 2);
    });
    // test shared candidate
    set_context(suite, [](TestSuite & suite, Unit & unit) {
        // avg should be 2
        std::vector<UniDimRecord> col = {
            UniDimRecord { 0  , 1 },
            UniDimRecord { 0.5, 1 },
            UniDimRecord { 1.5, 1 },
            UniDimRecord { 2.5, 1 },
            UniDimRecord { 3.0, 1 }
        };
        TestUniMap map;
        map.set_elements(col.begin(), col.end());
        unit.start(mark(suite), [&] {
            return test(map.collect_candidates(UniDimRecord{ 1.5, 0.4 }).size() == 3);
        });
        unit.start(mark(suite), [&] {
            return test(map.collect_candidates(UniDimRecord{ 2.2, 0.4 }).size() == 3);
        });
        unit.start(mark(suite), [&] {
            // we get *all* elements are candidate interactions
            return test(map.collect_candidates(UniDimRecord{ 1.8, 0.4 }).size() == 5);
        });
    });
    using TestBiMap = SpatialMap<BiDimRecord, BiDimMapFactory>;
    set_context(suite, [](TestSuite & suite, Unit & unit) {
        std::vector col = {
            BiDimRecord{  2,  2, 2, 2 },
            BiDimRecord{  4,  4, 2, 2 },
            BiDimRecord{ 10, 10, 2, 2 },
            BiDimRecord{ 12, 12, 2, 2 },
            BiDimRecord{  4,  7, 2, 2 },
            BiDimRecord{  2, 12, 3, 3 },
            BiDimRecord{  7, 12, 2, 3 }
        };
        for (auto & entry : col) {
            bool on_right  = entry.bounds.left > BiDimMapFactory::k_division;
            bool on_bottom = entry.bounds.top  > BiDimMapFactory::k_division;
            entry.name += on_bottom ? "b" : "t";
            entry.name += on_right  ? "r" : "l";
        }
#       if 0
        (() => {
            let col = [ [  2,  2, 2, 2 ],
                        [  4,  4, 2, 2 ],
                        [ 10, 10, 2, 2 ],
                        [ 12, 12, 2, 2 ],
                        [  4,  7, 2, 2 ],
                        [  2, 12, 3, 3 ],
                        [  7, 12, 2, 3 ] ];
            const context = $('#canvas').getContext('2d');
            const draw_divs = () => {
                    context.strokeStyle = '#F99';
                    context.beginPath();
                    context.moveTo(0*10, 8*10);
                    context.lineTo(16*10, 8*10);
                    context.closePath();
                    context.stroke();
                    context.beginPath();
                    context.moveTo(8*10, 0*10);
                    context.lineTo(8*10, 16*10);
                    context.closePath();
                    context.stroke();
            };
            const draw_rect = (e, outline_style, fill_style) => {
                outline_style = outline_style || '#F0F';
                fill_style    = fill_style    || '#000';
                context.fillStyle = outline_style;
                context.fillRect(e[0]*10 - 1, e[1]*10 - 1, e[2]*10 + 2, e[3]*10 + 2);
                context.fillStyle = fill_style;
                context.fillRect(e[0]*10, e[1]*10, e[2]*10, e[3]*10);
            };
            context.fillRect(0, 0, 400, 400);
            context.fillStyle = '#FFF';
            $('#canvas').getContext('2d').fillRect(0, 0, 400, 400);
            col.forEach(e => draw_rect(e));
            draw_rect([12, 13, 2, 2], '#F0F', '#FF0');
            draw_divs();
        })();
#       endif
        TestBiMap map;
        map.set_elements(col.begin(), col.end());
        unit.start(mark(suite), [&] {
            auto cands = map.collect_candidates(BiDimRecord{12, 13, 2, 2});
            return test(cands.size() == 3);
        });
        unit.start(mark(suite), [&] {
            auto cands = map.collect_candidates(BiDimRecord{4, 13, 2, 2});
            return test(cands.size() == 3);
        });
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
