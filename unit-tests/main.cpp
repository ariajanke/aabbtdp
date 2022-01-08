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

void do_sight_unit_tests(TestSuite &);

void do_integration_tests(TestSuite &);

int main() {
    TestSuite suite;
    suite.hide_successes();
    // helpers
    // the damn flag resets for every series :/
    auto k_test_functions = {
        do_CollisionEvent_tests,
        do_EventRecorder_tests,
        do_update_broad_boundries_tests,
        do_find_min_push_displacement_tests,
        do_trim_displacement_for_barriers_tests,
        do_trim_displacement_tests,
        do_trespass_occuring_tests,
        do_misc_tests,
        do_sight_unit_tests,
        do_integration_tests
    };
    bool all_successes = true;
    for (auto f : k_test_functions) {
        f(suite);
        if (!suite.has_successes_only())
            all_successes = false;
    }
    return !all_successes;
}
