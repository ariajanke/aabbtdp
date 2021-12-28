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

#include <common/TestSuite.hpp>
#include <common/Util.hpp>

#include <random>
#include <iostream>

#include "../src/helpers.hpp"

// helpers.hpp
// - CollisionEvent
// - EventRecorder
// - Helper Free Functions:
//   - update_broad_boundries
//   - absorb_nudge
//   - find_min_push_displacement
//   - trim_displacement_for_barriers
//   - trim_displacement
//   - trespass_occuring
//   - grow
//   - grow_by_displacement
//   - find_barrier_for_displacement
//   - displace
//   - large_displacement_step_count
//   - find_min_push_displacement_small
//   - trim_small_displacement
//   - values_from_displacement

namespace {

using Rng = std::default_random_engine;
using cul::ts::TestSuite, cul::ts::test, cul::ts::set_context, cul::ts::Unit,
      tdp::CollisionEvent, tdp::Rectangle, tdp::Vector, tdp::Real, tdp::Size;

#define mark MACRO_MARK_POSITION_OF_CUL_TEST_SUITE

using Entity = tdp::Entity;
class EntityMaker final {
public:
    EntityMaker() {
        std::iota(m_names.begin(), m_names.begin() + 26, 'a');
        std::iota(m_names.begin() + 26, m_names.begin() + 26*2, 'A');
    }

    Entity make_entity() {
        if (m_current == m_names.end()) {
            throw std::runtime_error("Ran out of names.");
        }
        return static_cast<void *>(&*m_current++);
    }

    static char get_name(Entity e)
        { return *reinterpret_cast<const char *>(e); }

private:
    using NamesArray = std::array<char, 26*2>;
    NamesArray m_names;
    NamesArray::iterator m_current = m_names.begin();
};

std::size_t hash(Entity e)
    { return std::hash<Entity>{}(e); }

template <typename Iter>
CollisionEvent random_col_event(Iter ent_beg, Iter ent_end, Rng & rng);

bool are_very_close(const Rectangle & a, const Rectangle & b);

bool are_very_close(const Vector & a, const Vector & b);

bool are_very_close(const Size & a, const Size & b);

bool are_very_close(Real a, Real b);

static constexpr const Real k_inf = std::numeric_limits<Real>::infinity();

} // end of <anonymous> namespace

// helpers.hpp
// - CollisionEvent
void do_CollisionEvent_tests(TestSuite &);

// - EventRecorder
void do_EventRecorder_tests(TestSuite &);

// - Helper Free Functions:
//   - update_broad_boundries
// make sure they grow correctly (that's it!)
void do_update_broad_boundries_tests(TestSuite &);

//   - absorb_nudge (too trivial to test!)
//   - find_min_push_displacement
// test both large and small
// and another direction
// and test huge
//   - find_min_push_displacement_small
// ideally we should be testing "side hit" as well (but it's use is disputed)
// default cases, equal corner, on sides
void do_find_min_push_displacement_tests(TestSuite &);

//   - trim_displacement_for_barriers
// high and low
// try and fail some assertions!!
// default case (no collision and non real barrier)
void do_trim_displacement_for_barriers_tests(TestSuite &);

//   - trim_displacement
// small and large and (default no hit)
// and test huge
// - trim_displacement_small
void do_trim_displacement_tests(TestSuite &);

//   - trespass_occuring
void do_trespass_occuring_tests(TestSuite &);

void do_misc_tests(TestSuite &);

// These are skipped:
//   - find_barrier_for_displacement
//   - displace
//   - values_from_displacement

void do_CollisionEvent_tests(TestSuite & suite) {
    using namespace tdp;
    suite.start_series("CollisionEvent");
    mark(suite).test([] {
        CollisionEvent a, b;
        return test(   a == b && !CollisionEvent::is_less_than(a, b)
                    && !CollisionEvent::is_less_than(b, a));
    });
    mark(suite).test([] {
        EntityMaker eman;
        auto a = eman.make_entity();
        auto b = eman.make_entity();
        auto c = eman.make_entity();
        bool c_ahead_b = hash(c) > hash(b);
        CollisionEvent a_b(a, b, CollisionEvent::k_push);
        CollisionEvent a_c(a, c, CollisionEvent::k_push);
        bool ab_lt_ac = CollisionEvent::is_less_than(a_b, a_c);
        if (c_ahead_b) {
            return test(ab_lt_ac);
        }
        return test(!ab_lt_ac);
    });
    mark(suite).test([] {
        EntityMaker eman;
        auto a = eman.make_entity();
        auto b = eman.make_entity();
        CollisionEvent a_b(a, b, CollisionEvent::k_push);
        return test(hash(a_b.first()) > hash(a_b.second()));
    });

    set_context(suite, [](TestSuite & suite, Unit & unit) {
        EntityMaker eman;
        std::array entities = {
            eman.make_entity(),
            eman.make_entity(),
            eman.make_entity()
        };
        std::sort(entities.begin(), entities.end(),
            [](const Entity & a, const Entity & b) { return hash(a) < hash(b); });
        // order is certain now!
        auto a = entities[0];
        auto b = entities[1];
        auto c = entities[2];
        CollisionEvent a_b(a, b, CollisionEvent::k_push);
        CollisionEvent a_c(a, c, CollisionEvent::k_push);
        CollisionEvent b_c(b, c, CollisionEvent::k_push);
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
        EntityMaker eman;
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
                std::cout << hash( col[idx - 1].first()) << " " << hash(col[idx - 1].second()) << " " << (col[idx - 1].type() == CollisionEvent::k_push) << std::endl;
                std::cout << hash( col[idx].first()) << " " << hash(col[idx].second()) << " " << (col[idx].type() == CollisionEvent::k_push) << std::endl;
                return test(false);
            }
        }
        return test(true);
    });
#   if 0
    suite.start_series("detail::find_min_push_displacement");
    // test small for push, resultant rectangle must not overlap the new
    // rectangle
    // test large push, results are non-overlapping both must move at least
    // n pixels
    // test push with large y value
    set_context(suite, [](TestSuite & suite, Unit & unit) {
        using namespace tdp;
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
        using namespace tdp;
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
#   endif
}

template <typename Func>
auto make_collision_handler(Func && f) {
    class Impl final : public tdp::EventHandler {
    public:
        Impl(Func && f): m_f(std::move(f)) {}
        bool check_accept_collision(Entity, Entity) const
            { return true; }
        void on_collision(Entity a, Entity b, bool push_occuring)
            { m_f(a, b, push_occuring); }
        void on_trespass(Entity, Entity) final {}
        void finalize_entry(Entity, Rectangle) final {}
    private:
        Func m_f;
    };
    return Impl{std::move(f)};
}

void do_EventRecorder_tests(TestSuite & suite) {
    using namespace tdp;
    suite.start_series("EventRecorder");
    set_context(suite, [](TestSuite & suite, Unit & unit) {
        EntityMaker maker;
        auto a = maker.make_entity();
        auto b = maker.make_entity();
        EventRecorder recorder;
        // base case
        unit.start(mark(suite), [&] {
            recorder.emplace_event(a, b, CollisionEvent::k_push);
            bool okay = false;
            auto handler = make_collision_handler([&okay] (Entity a, Entity b, bool) {
                okay =    EntityMaker::get_name(a) == 'a'
                       || EntityMaker::get_name(b) == 'a';
            });
            recorder.send_events(handler);
            return test(okay);
        });

        int how_many_events_with_a = 0;
        auto handler = make_collision_handler([&how_many_events_with_a]
            (Entity a, Entity b, bool)
        {
            how_many_events_with_a += int(   EntityMaker::get_name(a) == 'a'
                                          || EntityMaker::get_name(b) == 'a');
        });
        // filter dupelicate same frame
        unit.start(mark(suite), [&] {
            recorder.emplace_event(a, b, CollisionEvent::k_push);
            recorder.emplace_event(a, b, CollisionEvent::k_push);

            recorder.send_events(handler);
            return test(how_many_events_with_a == 1);
        });
        // filter dupelicate different consecutive frames
        unit.start(mark(suite), [&] {
            recorder.emplace_event(a, b, CollisionEvent::k_push);
            recorder.send_events(handler);
            recorder.emplace_event(a, b, CollisionEvent::k_push);
            recorder.send_events(handler);

            return test(how_many_events_with_a == 1);
        });
        // old becomes new again
        unit.start(mark(suite), [&] {
            recorder.emplace_event(a, b, CollisionEvent::k_push);
            recorder.send_events(handler);
            // one frame *without* submitting the event
            recorder.send_events(handler);
            recorder.emplace_event(a, b, CollisionEvent::k_push);
            recorder.send_events(handler);

            return test(how_many_events_with_a == 2);
        });
    });
}

void do_update_broad_boundries_tests(TestSuite & suite) {
    using namespace tdp;
    suite.start_series("update_broad_boundries");
    // not sure I need more than one...
    mark(suite).test([] {
        FullEntry entry;
        //EntityMaker maker;
        //entry.entity = maker.make_entity();
        entry.bounds = Rectangle(5, 5, 10, 10);
        entry.displacement = Vector(-5, 5);
        entry.nudge        = Vector( 0, 2);
        const auto board_bounds = compute_board_boundries(entry);
        return test(   are_very_close(board_bounds.low_x ,  0)
                    && are_very_close(board_bounds.low_y ,  5)
                    && are_very_close(board_bounds.high_x, 15)
                    && are_very_close(board_bounds.high_y, 22));
    });
    mark(suite).test([] {
        FullEntry entry;
        entry.bounds = Rectangle(5, 5, 10, 10);
        entry.growth = Size(5, -2);
        const auto board_bounds = compute_board_boundries(entry);
        // height unaffected... but must expand horizontally
        return test(   are_very_close(board_bounds.low_x ,  2.5)
                    && are_very_close(board_bounds.low_y ,  5  )
                    && are_very_close(board_bounds.high_x, 17.5)
                    && are_very_close(board_bounds.high_y, 15  ));
    });

}

void do_find_min_push_displacement_tests(TestSuite & suite) {
    using namespace tdp;
    using std::get;
    suite.start_series("find_min_push_displacement");
    // equal on corner case
    mark(suite).test([] {
        Rectangle a( 0,  0, 10, 10);
        Rectangle b(12, 12, 10, 10);
        Vector displacement(5, 5);
        auto r = get<Vector>(find_min_push_displacement(a, b, displacement));
        return test(are_very_close(r.x, 3) ^ are_very_close(r.y, 3));
    });
    set_context(suite, [](TestSuite & suite, Unit & unit) {
        Rectangle a(0 , 0, 10, 10);
        Rectangle b(12, 0, 10, 10);
        unit.start(mark(suite), [&] {
            // default
            Vector displacement(1, 0);
            auto push = get<Vector>(find_min_push_displacement(a, b, displacement));
            return test(are_very_close(push, Vector{}));
        });
        unit.start(mark(suite), [&] {
            // small
            auto push = get<Vector>(find_min_push_displacement(a, b,
                Vector(4, 0)));
            return test(are_very_close(push, Vector(2, 0)));
        });
        unit.start(mark(suite), [&] {
            // large (over the entire other rectangle)
            auto push = get<Vector>(find_min_push_displacement(a, b,
                Vector(23, 0)));
            return test(are_very_close(push, Vector(21, 0)));
        });
        unit.start(mark(suite), [&] {
            // huge (make the machine "sweat", this should be a non-linear
            // algorithm)
            return test(false);
            static constexpr const Real k_gap = 2;
            static constexpr const Real k_small_delta = 3;
            static auto adding_of_double_a_works = [] (Real a, Real b) {
                auto s  = a*2 + b;
                auto fm = std::fmod(a*2 + b, a);
                return are_very_close(std::fmod(a*2 + b, a), b);
            };
            Real working = 5; // maybe just need a different prime number
            while (   adding_of_double_a_works(working, k_gap        )
                   && adding_of_double_a_works(working, k_small_delta))
            { working *= 2; }
            auto push = get<Vector>(find_min_push_displacement(a, b,
                Vector(working + k_small_delta, 0)));
            return test(are_very_close(push, Vector(working - k_gap, 0)));
        });
    });

    // there's another case that's a bug witnessed with the demo
}

void do_trim_displacement_for_barriers_tests(TestSuite & suite) {
    suite.start_series("trim_displacement_for_barriers");
    // x-ways (real y, but non crossing)
    // x-ways, non-real y
    // x-ways and y-ways
    set_context(suite, [](TestSuite & suite, Unit & unit) {
        using namespace tdp;
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
        using std::pow, tdp::trim_displacement_for_barriers;
        for (int i = 0; true; ++i) {
            // max out at the resolution of the ones place
            // if addition here does not change the value of the floating
            // point, then I'll consider it "out of scope" of this function
            // this should break around i == 53 (on my machine anyhow)
            if (pow(2, i) + 1 == pow(2, i)) break;

            Rectangle trect{0, 0, Real(pow(2, i)), Real(pow(2, i))};
            // used narrowed constructors, have to cast explicitly
            Vector displacement{Real(pow(2, i) + 2), Real(-2)};
            Vector barrier     {Real(pow(2, i) + 1), Real(-1)};
            trim_displacement_for_barriers(trect, barrier, displacement);
        }
        // passes if no assertions fire
        return test(true);
    });

}

void do_trim_displacement_tests(TestSuite & suite) {
    using namespace tdp;
    suite.start_series("trim_displacement");
    set_context(suite, [](TestSuite & suite, Unit & unit) {
        Rectangle a(0 , 0, 10, 10);
        Rectangle b(12, 0, 10, 10);
        unit.start(mark(suite), [&] {
            // default
            Vector displacement(1, 0);
            trim_displacement(a, b, displacement);
            return test(are_very_close(displacement, Vector{1, 0}));
        });
        unit.start(mark(suite), [&] {
            // small
            Vector displacement(4, 0);
            trim_displacement(a, b, displacement);
            return test(are_very_close(displacement, Vector(2, 0)));
        });
        unit.start(mark(suite), [&] {
            // large (over the entire other rectangle)
            Vector displacement(23, 0);
            trim_displacement(a, b, displacement);
            return test(are_very_close(displacement, Vector(2, 0)));
        });
        unit.start(mark(suite), [&] {
            // huge
            return test(false);
            static constexpr const Real k_gap = 2;
            static auto adding_of_double_a_works = [] (Real a, Real b) {
                return are_very_close(std::fmod(a*2 + b, a), b);
            };
            Real working = 2;
            while (adding_of_double_a_works(working, k_gap))
                { working *= 2; }
            Vector displacement(working, 0);
            trim_displacement(a, b, displacement);
            return test(are_very_close(displacement, Vector(k_gap, 0)));
        });
    });
}

void do_trespass_occuring_tests(TestSuite & suite) {
    using namespace tdp;
    suite.start_series("trespass_occuring");
    set_context(suite, [](TestSuite & suite, Unit & unit) {
        Rectangle a(0 , 0, 10, 10);
        Rectangle b(12, 0, 10, 10);
        unit.start(mark(suite), [&] {
            // default
            return test(!trespass_occuring(a, b, Vector(1, 0)));
        });
        unit.start(mark(suite), [&] {
            // small
            return test(trespass_occuring(a, b, Vector(4, 0)));
        });
        unit.start(mark(suite), [&] {
            // large (over the entire other rectangle)
            return test(trespass_occuring(a, b, Vector(23, 0)));
        });
        unit.start(mark(suite), [&] {
            // huge
            return test(false);
            // I guess this becomes an arbitrarily choosen value of sorts
            static constexpr const Real k_gap = 2;
            static auto adding_of_double_a_works = [] (Real a, Real b) {
                return are_very_close(std::fmod(a*2 + b, a), b);
            };
            Real working = 2;
            while (adding_of_double_a_works(working, k_gap))
                { working *= 2; }
            return test(trespass_occuring(a, b, Vector(working, 0)));
        });
    });
}

void do_misc_tests(TestSuite & suite) {
    using namespace tdp;
    suite.start_series("misc Helpers");
    // - grow
    // grow one dim
    mark(suite).test([] {
        return test(are_very_close(
            grow(Rectangle(0, 0, 10, 10), Size(0, 5)).height, 15
        ));
    });
    // shrink another
    mark(suite).test([] {
        return test(are_very_close(
            grow(Rectangle(0, 0, 10, 10), Size(-3, 0)).width, 7
        ));
    });
    // - grow_by_displacement
    // one test case...
    mark(suite).test([] {
        return test(are_very_close(
            cul::size_of(grow_by_displacement(Rectangle(0, 0, 10, 10), Vector(-5, 2))),
            Size(15, 12)
        ));
    });
}

namespace {

static constexpr const Real k_very_close_error = 0.005;

template <typename Iter>
CollisionEvent random_col_event(Iter ent_beg, Iter ent_end, Rng & rng) {
    if (ent_beg == ent_end) return tdp::CollisionEvent();

    using IntDistri = std::uniform_int_distribution<int>;
    auto sel_a = ent_beg + IntDistri(0, ent_end - ent_beg - 1)(rng);
    auto sel_b = ent_beg + IntDistri(0, ent_end - ent_beg - 1)(rng);
    bool is_push = false;
    if (sel_a != sel_b) {
        is_push = IntDistri(0, 7)(rng) == 0;
    }
    return CollisionEvent(
        *sel_a,
        sel_a == sel_b ? Entity{} : *sel_b,
        is_push ? CollisionEvent::k_push : CollisionEvent::k_rigid);
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

bool are_very_close(const Size & a, const Size & b) {
    return cul::magnitude(a - b) < k_very_close_error;
}

bool are_very_close(Real a, Real b) {
    return cul::magnitude(a - b) < k_very_close_error;
}

} // end of <anonymous> namespace
