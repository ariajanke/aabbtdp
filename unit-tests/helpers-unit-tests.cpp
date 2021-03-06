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
      tdp::Rectangle, tdp::Vector, tdp::Real, tdp::Size;

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
#if 0
bool are_very_close(const Rectangle & a, const Rectangle & b);
#endif
bool are_very_close(const Vector & a, const Vector & b);

bool are_very_close(const Size & a, const Size & b);

bool are_very_close(Real a, Real b);

static constexpr const Real k_inf = std::numeric_limits<Real>::infinity();

// starting point for working needs to be different (prime number) than the delta
bool adding_of_double_a_works(Real a, Real b)
    { return are_very_close(std::fmod(a*2 + b, a), b); }

} // end of <anonymous> namespace

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

template <typename Func>
auto make_collision_handler(Func && f) {
    class Impl final : public tdp::EventHandler {
    public:
        Impl(Func && f): m_f(std::move(f)) {}
        bool check_accept_collision(Entity, Entity) const
            { return true; }
        void on_collision(Entity a, Entity b, bool push_occuring, OccurrenceType) final
            { m_f(a, b, push_occuring); }
        void on_trespass(Entity, Entity, OccurrenceType) final {}
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
            recorder.emplace_event(a, b, EventRecorder::k_push);
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
            recorder.emplace_event(a, b, EventRecorder::k_push);
            recorder.emplace_event(a, b, EventRecorder::k_push);

            recorder.send_events(handler);
            return test(how_many_events_with_a == 1);
        });
        // filter dupelicate different consecutive frames
        unit.start(mark(suite), [&] {
            recorder.emplace_event(a, b, EventRecorder::k_push);
            recorder.send_events(handler);
            recorder.emplace_event(a, b, EventRecorder::k_push);
            recorder.send_events(handler);

            return test(how_many_events_with_a == 1);
        });
        // old becomes new again
        unit.start(mark(suite), [&] {
            recorder.emplace_event(a, b, EventRecorder::k_push);
            recorder.send_events(handler);
            // one frame *without* submitting the event
            recorder.send_events(handler);
            recorder.emplace_event(a, b, EventRecorder::k_push);
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
            static constexpr const Real k_gap = 2;
            static constexpr const Real k_small_delta = 3;
            Real working = 5; // maybe just need a different prime number
            while (   adding_of_double_a_works(working, k_gap        )
                   && adding_of_double_a_works(working, k_small_delta))
            { working *= 2; }
            auto push = get<Vector>(find_min_push_displacement(a, b,
                Vector(working + k_small_delta, 0)));
            return test(are_very_close(push, Vector(working + (k_small_delta - k_gap), 0)));
        });
        // until out of the way
        unit.start(mark(suite), [&] {
            const Vector a_displc{20, -15};
            const Vector cor_push{(20*8.5) / 15, 0};
            const auto push = get<Vector>(find_min_push_displacement(a, b, a_displc));
            return test(are_very_close(push, cor_push));
        });
    });
    // observed failure 22-1-2
    // assertion fails: push direction is not compatible with the subject's
    // displacement
    mark(suite).test([] {
        Vector displc{2.5737990837420464, 0};
        Rectangle other{130.96957313685118,80.722277825726977,10,10};
        Rectangle rect {120.10942830086175,70.722500269287721,10,10};
        auto push = std::get<Vector>(find_min_push_displacement(rect, other, displc));
        return test(push.x != 0 && push.y == 0);
    });
    // I can always add more tests later c:
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
            HitSide gv;
            std::tie(displacement, gv) = trim_displacement_for_barriers(trect, barrier, displacement);
            return test(   gv.horizontal == k_right
                        && gv.vertical   == k_direction_count
                        && are_very_close(displacement, Vector{5, -5}));
        });
        unit.start(mark(suite), [&] {
            Vector barrier{15, k_inf};
            HitSide gv;
            std::tie(displacement, gv) = trim_displacement_for_barriers(trect, barrier, displacement);
            return test(   gv.horizontal == k_right
                        && gv.vertical   == k_direction_count
                        && are_very_close(displacement, Vector{5, -5}));
        });
        unit.start(mark(suite), [&] {
            Vector barrier{15, -3};
            HitSide gv;
            std::tie(displacement, gv) = trim_displacement_for_barriers(trect, barrier, displacement);
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
            displacement = std::get<Vector>(trim_displacement(a, b, displacement));
            return test(are_very_close(displacement, Vector{1, 0}));
        });
        unit.start(mark(suite), [&] {
            // small
            Vector displacement(4, 0);
            displacement = std::get<Vector>(trim_displacement(a, b, displacement));
            return test(are_very_close(displacement, Vector(2, 0)));
        });
        unit.start(mark(suite), [&] {
            // large (over the entire other rectangle)
            Vector displacement(23, 0);
            displacement = std::get<Vector>(trim_displacement(a, b, displacement));
            return test(are_very_close(displacement, Vector(2, 0)));
        });
        unit.start(mark(suite), [&] {
            // huge
            static constexpr const Real k_gap = 2;
            Real working = 3;
            while (adding_of_double_a_works(working, k_gap))
                { working *= 2; }
            Vector displacement(working, 0);
            displacement = std::get<Vector>(trim_displacement(a, b, displacement));
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
            // I guess this becomes an arbitrarily choosen value of sorts
            static constexpr const Real k_gap = 2;
            static auto adding_of_double_a_works = [] (Real a, Real b) {
                return are_very_close(std::fmod(a*2 + b, a), b);
            };
            Real working = 3;
            while (adding_of_double_a_works(working, k_gap))
                { working *= 2; }
            return test(trespass_occuring(a, b, Vector(working, 0)));
        });
    });
    mark(suite).test([] {
        // confirmed... no overlap... ever!
        Vector displc{138,-54};
        Vector origin{193.19999999999999, 163.19999999999999};
        Rectangle other = displace(Rectangle{291.19999999999999,163.19999999999999,20,20}, -origin);
        Rectangle rect  = displace(Rectangle{193.19999999999999, 193.19999999999999, 4, 4}, -origin);
        return test(!trespass_occuring(rect, other, displc));
    });
    #   if 0
    $('body').html('<div id="a"></div><div id="b"></div>');
    const do_show = (et, rate) => {
        let time = 0;
        const to = () => {
            time += et*rate;
            if (time > 1) time = 1;
            update_divs(time);
            if (time >= 1) return;
            setTimeout(to, et / 1000);
        };
        to();
    };
    const update_divs = t => {
        const displc = {x: 138, y: -54};
        const other = {x: 291.19999999999999, y: 163.19999999999999, w: 20,h:20};
        const rect  = {x: 193.19999999999999, y: 193.19999999999999, w: 4, h:4};
        const do_rect = (tag, rect) => {
            return tag.css('left', rect.x + 'px').css('top', rect.y + 'px')
                      .css('width', rect.w + 'px').css('height', rect.w + 'px');
        };
        const displace = (rect, displc, t) => {
            return { x: rect.x + displc.x*t, y: rect.y + displc.y*t,
                     w: rect.w, h: rect.h };
        };

        do_rect($('#a'), other).css('background-color', '#F33').css('position', 'absolute');
        do_rect($('#b'), displace(rect, displc, t))
            .css('background-color', '#3F3').css('position', 'absolute');
    };
        displc	@0x5591297b8bb8	tdp::Vector &
            x	138	double
            y	-54	double
        other	@0x5591297e3c68	tdp::Rectangle &
            height	20	double
            left	291.19999999999999	double
            top	163.19999999999999	double
            width	20	double
        rect	@0x5591297b8b88	tdp::Rectangle &
            height	4	double
            left	193.19999999999999	double
            top	193.19999999999999	double
            width	4	double
#   endif
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
    mark(suite).test([] {
        struct TestStruct final {
            TestStruct() {}
            explicit TestStruct(int m): v(m) {}
            int v = 1;
        };
        struct BadHasher final {
            std::size_t operator ()(void *) const { return 5; }
        };

        using rigtorp::HashMap, std::make_tuple, std::make_pair;
        using TestHashMap = HashMap<void *, TestStruct, BadHasher>;
        TestHashMap hashmap{8, nullptr};

        const auto [k_a_key, k_b_key, k_c_key, k_d_key] = [] {
            static std::array<uint8_t, 4> k_key_addrs;
            static auto void_ptr_of = [](int idx) -> void *
                { return &k_key_addrs[idx]; };
            return make_tuple(void_ptr_of(0), void_ptr_of(1), void_ptr_of(2), void_ptr_of(3));
        } ();
        hashmap.insert(make_pair(k_a_key, TestStruct{1}));
        hashmap.insert(make_pair(k_b_key, TestStruct{2}));
        hashmap.insert(make_pair(k_c_key, TestStruct{3}));
        hashmap.insert(make_pair(k_d_key, TestStruct{4}));

        int found_el_with_three = 0;
        int found_el_with_four  = 0;
        for (auto itr = hashmap.begin(); itr != hashmap.end(); ) {
            assert(itr->first);
            switch (itr->second.v) {
            case 2:
#               ifndef MACRO_USE_OLD_RIGTORP_HASHMAP
                // this *does* work
                itr = hashmap.erase(itr);
#               elif 0
                // explicit increment
                // this also doesn't work, as we could skip a valid next
                // element
                hashmap.erase(itr);
                ++itr;
#               else
                // old style of re using old iterator
                // this doesn't work as what if old iterator just goes to
                // empty key?
                //
                // in this test case though wrap around still causes undesired
                // behavior
                hashmap.erase(itr);
#               endif
                continue; // -> for loop
            case 3: ++found_el_with_three; break;
            case 4: ++found_el_with_four ; break;
            default: break;
            }
            ++itr;
        }

        return test(found_el_with_three == 1 && found_el_with_four == 1);
    });
}

namespace {

static constexpr const Real k_very_close_error = 0.005;
#if 0
bool are_very_close(const Rectangle & a, const Rectangle & b) {
    using cul::magnitude;
    //static constexpr const Real k_error = 0.005;
    return    magnitude(a.left   - b.left  ) < k_very_close_error
           && magnitude(a.top    - b.top   ) < k_very_close_error
           && magnitude(a.width  - b.width ) < k_very_close_error
           && magnitude(a.height - b.height) < k_very_close_error;
}
#endif
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
