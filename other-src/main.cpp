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
#include "../src/detail.hpp"
#include "../src/PartitionBoxMap.hpp"

#include <common/TestSuite.hpp>

#include <iostream>
#include <random>

#include <cassert>

namespace {

void run_td_physics_tests();
using EntityA = ecs::Entity<Velocity, Rectangle, MapLimits, Name, ColInfo, Growth, Layer>;
using EntityManagerA = EntityA::ManagerType;

} // end of <anonymous> namespace

int main() {
    std::cout << "Test Entity size " << EntityA::k_component_table_size / sizeof(void *)
              << " pointers with " << EntityA::k_number_of_components_inlined << " components inlined." << std::endl;
    run_td_physics_tests();
#   ifdef MACRO_BUILD_DEMO
    run_demo();
#   endif
    return 0;
}

namespace {


class ColSystem final : public EntityA::SystemType {
public:
    static constexpr const Real k_et_value = 1. / 60.;

    ColSystem() {
        m_handle->set_collision_matrix(make_collision_matrix());
    }

private:
    class DefaultEventHandler final : public tdp::EventHandler {
        using EntityRef = ecs::EntityRef;
        bool check_accept_collision(EntityRef, EntityRef) const final { return true; }

        void on_collision(EntityRef a, EntityRef b, bool) final {
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

        void on_trespass(EntityRef, EntityRef) final {}

        void finalize_entry(EntityRef eref, Rectangle rect) final
            { EntityA { eref }.get<Rectangle>() = rect; }
    };

    void update(const ContainerView & view) final {
        for (auto & e : view) {
            m_handle->update_entry(to_tdp_entry(e, k_et_value));
        }
        DefaultEventHandler def_handler;
        m_handle->run(def_handler);
    }

    tdp::TdpHandlerPtr m_handle = tdp::TopDownPhysicsHandler::make_instance();
};

template <typename LessThanFunc>
class DerivedComparitors {
public:
    //explicit DerivedComparitors(std::enable_if_t<std::is_class_v<LessThanFunc>, LessThanFunc> && f): m_f(std::move(f)) {}
    //explicit DerivedComparitors(std::enable_if_t<std::is_pointer_v<LessThanFunc>, LessThanFunc> fptr): m_f(fptr) {}

    explicit DerivedComparitors(LessThanFunc fptr): m_f(fptr) {}

    // "<" to >=, >, ==, !=, <=
    template <typename T>
    bool is_gte(const T & a, const T & b) const { return !m_f(a, b); }

    template <typename T>
    bool is_eq(const T & a, const T & b) const
        { return !m_f(a, b) && !m_f(b, a); }

    template <typename T>
    bool is_neq(const T & a, const T & b) const
        { return !is_eq(a, b); }

    template <typename T>
    bool is_gt(const T & a, const T & b) const
        { return !m_f(a, b) && is_neq(a, b); }

    template <typename T>
    bool is_lte(const T & a, const T & b) const
        { return m_f(a, b) || is_eq(a, b); }

private:
    LessThanFunc m_f;
};

template <typename Iter>
tdp::detail::CollisionEvent random_col_event(Iter ent_beg, Iter ent_end, std::default_random_engine & rng) {
    if (ent_beg == ent_end) return tdp::detail::CollisionEvent();

    using IntDistri = std::uniform_int_distribution<int>;
    auto sel_a = ent_beg + IntDistri(0, ent_end - ent_beg - 1)(rng);
    auto sel_b = ent_beg + IntDistri(0, ent_end - ent_beg - 1)(rng);
    bool is_push = false;
    if (sel_a != sel_b) {
        is_push = IntDistri(0, 7)(rng) == 0;
    }
    return tdp::detail::CollisionEvent(
        *sel_a,
        sel_a == sel_b ? ecs::EntityRef() : *sel_b,
        is_push);
}

using cul::Grid;
auto make_tdp_handler() { return tdp::TopDownPhysicsHandler::make_instance(); }

void do_collision_matrix_tests(cul::ts::TestSuite &);
void do_sweep_and_prune_tests(cul::ts::TestSuite &);

bool are_very_close(const Rectangle & a, const Rectangle & b) {
    using cul::magnitude;
    static constexpr const Real k_error = 0.005;
    return    magnitude(a.left   - b.left  ) < k_error
           && magnitude(a.top    - b.top   ) < k_error
           && magnitude(a.width  - b.width ) < k_error
           && magnitude(a.height - b.height) < k_error;
}

void run_td_physics_tests() {
    using cul::ts::TestSuite;
    using cul::ts::test;
    TestSuite suite;
    suite.start_series("td physics - detail - CollisionEvent");
    {
    using namespace tdp::detail;
    suite.test([] {
        CollisionEvent a, b;
        return test(   a == b && !CollisionEvent::is_less_than(a, b)
                    && !CollisionEvent::is_less_than(b, a));
    });
    suite.test([] {
        EntityManagerA eman;
        auto a = eman.create_new_entity();
        auto b = eman.create_new_entity();
        auto c = eman.create_new_entity();
        bool c_ahead_b = c.hash() > b.hash();
        CollisionEvent a_b(a, b, true);
        CollisionEvent a_c(a, c, true);
        bool ab_lt_ac = CollisionEvent::is_less_than(a_b, a_c);
        if (c_ahead_b) {
            return test(ab_lt_ac);
        }
        return test(!ab_lt_ac);
    });
    suite.test([] {
        EntityManagerA eman;
        auto a = eman.create_new_entity();
        auto b = eman.create_new_entity();
        CollisionEvent a_b(a, b, true);
        return test(a_b.first().hash() < a_b.second().hash());
    });
    suite.test([] {
        EntityManagerA eman;
        std::array entities = {
            eman.create_new_entity(),
            eman.create_new_entity(),
            eman.create_new_entity()
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
        bool ab_ac = CollisionEvent::is_less_than(a_b, a_c);
        bool ac_bc = CollisionEvent::is_less_than(a_c, b_c);
        return test(ab_ac && ac_bc);
    });
    // strict weak ordering... from "<" all others follow
    // "<" to >=, <, ==, !=, <=

    static auto const ce_comps = DerivedComparitors(CollisionEvent::is_less_than);
    suite.test([] {
        EntityManagerA eman;
        std::array entities = {
            eman.create_new_entity(), eman.create_new_entity(), eman.create_new_entity()
        };
        std::sort(entities.begin(), entities.end(),
            [](const EntityA & a, const EntityA & b) { return a.hash() < b.hash(); });
        auto a = entities[0];
        auto b = entities[1];
        auto c = entities[2];
        CollisionEvent a_b(a, b, true);
        CollisionEvent a_c(a, c, true);
        CollisionEvent b_c(b, c, true);

        bool gte_passes = ce_comps.is_gte(a_b, a_b) && ce_comps.is_gte(b_c, a_b);
        bool gt_passes  = !ce_comps.is_gt(a_b, a_b) && ce_comps.is_gt(b_c, a_b);
        bool eq_passes  = ce_comps.is_eq(a_b, a_b) && !ce_comps.is_eq(b_c, a_b);
        bool neq_passes = !ce_comps.is_neq(a_b, a_b) && ce_comps.is_neq(b_c, a_b);
        bool lte_passes = ce_comps.is_lte(a_b, a_b) && ce_comps.is_lte(a_c, b_c);
        return test(   gte_passes && gt_passes && eq_passes && neq_passes
                    && lte_passes);
    });
    suite.test([] {
        std::default_random_engine rng { 0x912EA01 };
        EntityManagerA eman;
        std::array entities = {
            eman.create_new_entity(), eman.create_new_entity(), eman.create_new_entity(),
            //eman.create_new_entity(), eman.create_new_entity(), eman.create_new_entity()
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
    }
    do_collision_matrix_tests(suite);
    suite.start_series("entry validity");
    suite.test([] {
        auto uptr = tdp::TopDownPhysicsHandler::make_instance();
        uptr->set_collision_matrix(make_collision_matrix());
        tdp::Entry entry;
        entry.bounds = Rectangle(0, 0, 10, -10);
        try {
            uptr->update_entry(entry);
        } catch (std::invalid_argument &) {
            return test(true);
        }
        return test(false);
    });
    suite.test([] {
        auto uptr = tdp::TopDownPhysicsHandler::make_instance();
        uptr->set_collision_matrix(make_collision_matrix());
        tdp::Entry entry;
        entry.bounds = Rectangle(0, 0, 10, 10);
        entry.displacement = Vector(0, std::numeric_limits<Real>::infinity());
        entry.collision_layer = layers::k_block;
        try {
            uptr->update_entry(entry);
        } catch (std::invalid_argument &) {
            return test(true);
        }
        return test(false);
    });
    suite.test([] {
        auto uptr = tdp::TopDownPhysicsHandler::make_instance();
        uptr->set_collision_matrix(make_collision_matrix());
        tdp::Entry entry;
        entry.bounds = Rectangle(0, 0, 10, 10);
        entry.growth = Size2(10, std::numeric_limits<Real>::infinity());
        entry.collision_layer = layers::k_floor_mat;
        try {
            uptr->update_entry(entry);
        } catch (std::invalid_argument &) {
            return test(true);
        }
        return test(false);
    });
    suite.start_series("td physics - one on one entity");

    // unit tests
    // - one entity, hits a map wall
    suite.test([] {
        EntityManagerA eman;
        auto e = eman.create_new_entity();
        e.add<Velocity >() = Velocity(2.5 / ColSystem::k_et_value, 0);
        e.add<MapLimits>() = MapLimits(10., 0);
        e.add<Rectangle>() = Rectangle(0., 0., 8., 8.);
        ColSystem colsys;
        eman.process_deletion_requests();
        eman.run_system(colsys);
        return test(e.get<ColInfo>().hit_wall);
    });

    // - one entity, trepasses onto another by displacement
    suite.test([] {
        return test(false);
    });
    // - one entity, trepasses onto another by growth
    suite.test([] {
        return test(false);
    });
    // - one entity, collides with another (not pushable)
    suite.test([] {
        return test(false);
    });
    // - one entity, collides with one while another is present
    // - one entity, pushes another
    // more test ideas:
    // multi frame tests
    // multi entity tests

    suite.start_series("growth/shrink tests");
    // test growth
    suite.test([] {
        EntityManagerA eman;
        auto e = eman.create_new_entity();
        e.add<Growth>() = Size2(10, 10);
        e.add<Rectangle>() = Rectangle(10, 10, 10, 10);
        e.add<Layer>() = layers::k_floor_mat;
        ColSystem colsys;
        for (int i = 0; i != int(std::ceil(1. / ColSystem::k_et_value)); ++i) {
            eman.process_deletion_requests();
            eman.run_system(colsys);
        }
        return test(are_very_close(e.get<Rectangle>(), Rectangle(5, 5, 20, 20)));
    });
    // test shrink
    suite.test([] {
        EntityManagerA eman;
        auto e = eman.create_new_entity();
        e.add<Growth>() = Size2(-5, -5);
        e.add<Rectangle>() = Rectangle(10, 10, 10, 10);
        e.add<Layer>() = layers::k_floor_mat;
        ColSystem colsys;
        for (int i = 0; i != int(std::ceil(1. / ColSystem::k_et_value)); ++i) {
            eman.process_deletion_requests();
            eman.run_system(colsys);
        }
        return test(are_very_close(e.get<Rectangle>(), Rectangle(7.5, 7.5, 5, 5)));
    });
    // test shrink to zero
    suite.test([] {
        EntityManagerA eman;
        auto e = eman.create_new_entity();
        e.add<Growth>() = Size2(-15, -15);
        e.add<Rectangle>() = Rectangle(10, 10, 10, 10);
        e.add<Layer>() = layers::k_floor_mat;
        ColSystem colsys;
        for (int i = 0; i != int(std::ceil(1. / ColSystem::k_et_value)); ++i) {
            eman.process_deletion_requests();
            eman.run_system(colsys);
        }
        return test(are_very_close(e.get<Rectangle>(), Rectangle(15, 15, 0, 0)));
    });
    // test growth must not be solid on any layer
    suite.test([] {
        EntityManagerA eman;
        auto e = eman.create_new_entity();
        e.add<Growth>() = Size2(10, 10);
        e.add<Rectangle>() = Rectangle(10, 10, 10, 10);
        e.add<Layer>() = layers::k_block;
        auto uptr = tdp::TopDownPhysicsHandler::make_instance();
        uptr->set_collision_matrix(make_collision_matrix());
        try {
            uptr->update_entry(to_tdp_entry(e, 1. / 60.));
        } catch (std::invalid_argument &) {
            return test(true);
        }
        return test(false);
    });
    // make sure a growing entity hits another
    suite.test([] {
        return test(false);
    });
    do_sweep_and_prune_tests(suite);
}

// ----------------------------------------------------------------------------

void do_collision_matrix_tests(cul::ts::TestSuite & suite) {
    using cul::ts::TestSuite;
    using cul::ts::test;
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
        auto e = eman.create_new_entity();
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
    using cul::ts::test;
    suite.start_series("sweep prune map tests");
    static constexpr const int k_fork_thershold = tdp::detail::k_pbm_partition_thershold;
#   if 0 // idk what to do with this test case anymore
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
    suite.test([] {
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
    suite.test([] {
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

} // end of <anonymous> namespace
