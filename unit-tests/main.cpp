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

// We want to cover as much code as possible...

// helpers...
// CollisionHandler...

#include <common/TestSuite.hpp>
#include <common/Util.hpp>

#include <random>

#define MACRO_AABBTDP_SHOW_DETAILS_HELPERS
#include "../src/CollisionHandler.hpp"

// A todo list of functions and features... of course!
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
// CollisionHandler.hpp(?)
// We can do much of our testing of this, using the Quadratic/Naive
// implementation.
// - entry validation
// - matrix validation
// Implementations...
// (design note: each implementation should have illustrative methods)
// Quadratic...
// - Allow any algorithm to be used in quadratics "synethesis" test cases
// Sweep...
// - Make sure sweep is following order/form of steps like quadratic
//   - Sweep must only be "an optimization"
// Grid
// - helper free functions
//   - find_rectangle_range
// - check that no dupelicates get through
// - run tests along cell boundries
namespace {

using Rng = std::default_random_engine;
using cul::ts::TestSuite, cul::ts::test, cul::ts::set_context, cul::ts::Unit,
      cul::Grid, tdp::CollisionEvent;

void do_helper_tests          (TestSuite &);
void do_collision_matrix_tests(TestSuite &);
void do_td_physics_tests      (TestSuite &);

#define mark MACRO_MARK_POSITION_OF_CUL_TEST_SUITE

} // end of <anonymous> namespace

void do_CollisionEvent_tests(TestSuite &);
void do_EventRecorder_tests(TestSuite &);
void do_update_broad_boundries_tests(TestSuite &);
void do_find_min_push_displacement_tests(TestSuite &);
void do_trim_displacement_for_barriers_tests(TestSuite &);
void do_trim_displacement_tests(TestSuite &);
void do_trespass_occuring_tests(TestSuite &);
void do_misc_tests(TestSuite &);

int main() {
    TestSuite suite;
    suite.hide_successes();
    // helpers
    do_CollisionEvent_tests(suite);
    do_EventRecorder_tests(suite);
    do_update_broad_boundries_tests(suite);
    do_find_min_push_displacement_tests(suite);
    do_trim_displacement_for_barriers_tests(suite);
    do_trim_displacement_tests(suite);
    do_trespass_occuring_tests(suite);
    do_misc_tests(suite);

#   if 0
    do_helper_tests(suite);
    do_collision_matrix_tests(suite);
    do_td_physics_tests(suite);
#   endif
    return 0;
}

namespace {

void do_helper_tests(TestSuite & suite) {
#   if 0 // outmoded
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
#   endif

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

} // end of <anonymous> namespace
