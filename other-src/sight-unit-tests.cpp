#include "../src/sight-detail.hpp"

#include <common/TestSuite.hpp>

#include <iostream>

#include <cassert>

#define mark MACRO_MARK_POSITION_OF_CUL_TEST_SUITE

namespace tdp {

namespace detail {

PolarVector operator + (const PolarVector & lhs, const PolarVector & rhs)
    { return PolarVector{lhs.r + rhs.r, lhs.theta + rhs.theta}; }

} // end of detail namespace -> into ::tdp

} // end of tdp namespace

namespace {

using cul::ts::TestSuite, cul::ts::test, cul::magnitude;
using UnitTestFunctions = tdp::detail::SightingUnitTestFunctions;
using Entry = tdp::Sighting::Entry;
using tdp::Real, tdp::Rectangle, tdp::Vector, tdp::detail::ImageEntry,
      tdp::detail::PolarVector, tdp::detail::to_cartesian,
      tdp::detail::completely_overlaps_source;

Entry make_entry(const Rectangle & rect) {
    Entry e;
    e.bounds = rect;
    return e;
}

bool are_very_close(const Vector & a, const Vector & b)
    { return magnitude(a - b) < 0.005; }

bool are_very_close(Real a, Real b)
    { return magnitude(a - b) < 0.005; }
#if 0
void print_image(const ImageEntry & image) {
    std::cout << "low theta " << PolarVector{image.anchor_low}.theta
              << " - high theta " << PolarVector{image.anchor_high}.theta << std::endl;
}
#endif
bool has_valid_positions(const ImageEntry & image) {
    if (tdp::detail::completely_overlaps_source(image)) return true;
    return PolarVector{image.anchor_low}.theta < PolarVector{image.anchor_high}.theta;
}

} // end of <anonymous> namespace

void do_sight_unit_tests(TestSuite & suite) {
    static const auto inst = UnitTestFunctions::make_instance();
#   if 0 // this keeps changing, which makes me hesitent to make tests for
    static const auto crosses_anti_anchor_line = inst.crosses_anti_anchor_line;
    suite.start_series("sight - crosses anti anchor line");
#   endif
    static const auto make_image = inst.make_image;
    suite.start_series("sight - make_image");
    mark(suite).test([] {
        auto image = make_image(Vector{5, 5}, make_entry(Rectangle{ 0, 0, 10, 10 }));
        return test(tdp::detail::completely_overlaps_source(image));
    });
    // values depend on anchor
    // so if there are new failures, and the anchor value has changed, that
    // could explain it
    assert(are_very_close(inst.make_anchor(), Vector{1, 0}));
    mark(suite).test([] {
        // image directly above
        // low = br, high = bl
        Rectangle rect{ -5, -10, 10, 8 };
        auto image = make_image(Vector{0, 0}, make_entry(rect));
        return test(   are_very_close(image.anchor_high, Vector{ 5, -2})
                    && are_very_close(image.anchor_low , Vector{-5, -2}));
    });

    mark(suite).test([] {
        // image directly above
        // low = br, high = bl
        Rectangle rect{ -5, -10, 10, 8 };
        auto image = make_image(Vector{0, 0}, make_entry(rect));
        return test( has_valid_positions(image) );
    });

    // try a couple of different angles, then an additional that crosses the anti-anchor
    mark(suite).test([] {
        // image on the top-right
        // low is br, high is tl
        auto image = make_image(Vector{0, 0}, make_entry(Rectangle{5, -10, 5, 5}));
        return test(   are_very_close(image.anchor_high, Vector{10, -5})
                    && are_very_close(image.anchor_low , Vector{5, -10}));
    });
    // try one with a vastly different point of observation
    // images need to be relative to the point of observation
    mark(suite).test([] {
        // image on the top-right
        // low is br, high is tl
        Vector obsv{-100, 100};
        auto image = make_image(obsv, make_entry(Rectangle{5, -10, 5, 5}));
        return test(   are_very_close(image.anchor_high, Vector{10, -5} - obsv)
                    && are_very_close(image.anchor_low , Vector{5, -10} - obsv));
    });
    mark(suite).test([] {
        // image is on the bottom-left
        // low is tl, high is br
        // the anchors should sweep from low to high in the positive radian
        // direction
        auto image = make_image(Vector{0, 0}, make_entry(Rectangle{-10, 5, 5, 5}));
        return test(   are_very_close(image.anchor_high, Vector{-10, 5})
                    && are_very_close(image.anchor_low , Vector{-5, 10}));
    });
    mark(suite).test([] {
        // image is on the left
        // tr is low, br is high
        Vector obsv{100, 0};
        auto image = make_image(obsv, make_entry(Rectangle{10, -5, 10, 10}));
        return test(   are_very_close(image.anchor_high, Vector{20, -5} - obsv)
                    && are_very_close(image.anchor_low , Vector{20,  5} - obsv));
    });

    suite.start_series("sight - find_portion_overlapped");
    static const auto find_portion_overlapped = inst.find_portion_overlapped;
    using PolVec = tdp::detail::PolarVector;
    mark(suite).test([] {
        // object is behind the subject (must have no effect)
        return test(find_portion_overlapped(PolVec{2, 1.2}, PolVec{2, 1.5},
                                            PolVec{3, 1.2}, PolVec{3, 1.5}) == 0);
    });
    mark(suite).test([] {
        // complete (though barely) overlap
        return test(find_portion_overlapped(PolVec{3, 1.2}, PolVec{3, 1.5},
                                            PolVec{2, 1.2}, PolVec{2, 1.5}) == 1);
    });
    constexpr const Real k_pi = cul::k_pi_for_type<Real>;
    constexpr const Real k_test_arc_len = k_pi / 8;
    constexpr const Real k_test_arc_pos = k_pi / 2;
    mark(suite).test([] {
        PolVec sub_start{3, k_test_arc_pos + k_test_arc_len};
        PolVec obj_start{2, k_test_arc_pos + k_test_arc_len*0.4};
        return test(are_very_close(
            find_portion_overlapped(sub_start, sub_start + PolVec{0, k_test_arc_len},
                                    obj_start, obj_start + PolVec{0, k_test_arc_len}),
            0.4));
    });
    // overlap in the other direction (test 4)
    mark(suite).test([] {
        // nts: remember this is the portion overlapped, not portion exposed
        PolVec sub_start{3, k_test_arc_pos + k_test_arc_len*0.6};
        PolVec obj_start{2, k_test_arc_pos + k_test_arc_len};
        return test(are_very_close(
            find_portion_overlapped(sub_start, sub_start + PolVec{0, k_test_arc_len},
                                    obj_start, obj_start + PolVec{0, k_test_arc_len}),
            0.6));
    });
    // case where subject appears larger than object, but behind object (ofc)
    mark(suite).test([] {
        PolVec sub_start{3, k_test_arc_pos + k_test_arc_len};
        PolVec obj_start{2, k_test_arc_pos + k_test_arc_len*1.3};
        return test(are_very_close(
            find_portion_overlapped(sub_start, sub_start + PolVec{0, k_test_arc_len},
                                    obj_start, obj_start + PolVec{0, k_test_arc_len*0.4}),
            0.4));
    });
    // case where crossing the anti-anchor (pi/-pi) line
    mark(suite).test([] {
        PolVec sub_start{3, -k_pi + k_test_arc_len*0.75};
        PolVec obj_start{2, -k_pi + k_test_arc_len*0.15};
        return test(are_very_close(
            find_portion_overlapped(sub_start, sub_start + PolVec{0, k_test_arc_len},
                                    obj_start, obj_start + PolVec{0, k_test_arc_len}),
            0.4));
    });
    // no overlap, but object is infront of subject
    mark(suite).test([] {
        PolVec sub_start{3, k_test_arc_pos + k_test_arc_len};
        PolVec obj_start{2, k_test_arc_pos - k_test_arc_len*0.1};
        return test(
            find_portion_overlapped(sub_start, sub_start + PolVec{0, k_test_arc_len},
                                    obj_start, obj_start + PolVec{0, k_test_arc_len})
            == 0);
    });
    // these tests are a lot harder to test for correctness
    // (because of the use of cartesian coords)
    mark(suite).test([] {
        Vector sub_left{-2, -3}, sub_right{5, -7};
        Vector obj_left{-4, -7}, obj_right{5, -3};
        auto intx = cul::find_intersection(sub_left, sub_right, obj_left, obj_right);
        assert(intx != cul::get_no_solution_sentinel<Vector>());
        // I need to be careful when I have an accusitive
        auto cor_por = magnitude(intx - sub_right) / magnitude(sub_left - sub_right);
        return test(are_very_close(
            find_portion_overlapped(PolVec{sub_left}, PolVec{sub_right},
                                    PolVec{obj_left}, PolVec{obj_right}),
            cor_por));
    });
    // the last and "butterfly" overlap test
    mark(suite).test([] {
        Vector sub_left{-5, -6}, sub_right{2, -3};
        Vector obj_left{-2, -3}, obj_right{5, -6};
        auto intx = cul::find_intersection(sub_left, sub_right, obj_left, obj_right);
        assert(intx != cul::get_no_solution_sentinel<Vector>());
        auto por_part = magnitude(intx - sub_right) / magnitude(sub_left - sub_right);
        auto por = find_portion_overlapped(PolVec{sub_left}, PolVec{sub_right},
                                           PolVec{obj_left}, PolVec{obj_right});
        return test(are_very_close(por, por_part));
    });
    mark(suite).test([] {
        PolVec sublow{3, k_test_arc_pos}, subhigh{3, k_test_arc_pos};
        PolVec objlow{2, k_test_arc_pos/* - k_test_arc_len*0.5*/}, objhigh{2, k_test_arc_pos + k_test_arc_len*0.5};
        return test(find_portion_overlapped(sublow, subhigh, objlow, objhigh) == 1);
    });
    static const auto update_for_possible_obstruction = inst.update_for_possible_obstruction;
    suite.start_series("sight - update obstruction");
    mark(suite).test([] {
        ImageEntry subject;
        ImageEntry object;
        PolarVector sublow{3, k_test_arc_pos };
        PolarVector subhigh{3, k_test_arc_pos + k_test_arc_len};
        PolarVector objlow{2, k_test_arc_pos - k_test_arc_len*0.5};
        PolarVector objhigh{2, k_test_arc_pos + k_test_arc_len*0.5};
        subject.anchor_low  = to_cartesian(sublow);
        subject.anchor_high = to_cartesian(subhigh);
        object.anchor_low  = to_cartesian(objlow);
        object.anchor_high = to_cartesian(objhigh);
        assert(has_valid_positions(subject) && has_valid_positions(object));
        update_for_possible_obstruction(subject, object);
        return test(are_very_close(subject.visibility, 0.5));
    });
    mark(suite).test([] {
        ImageEntry subject;
        ImageEntry object;
        object.opactity = 0.5;
        PolarVector sublow{3, k_test_arc_pos - k_test_arc_len*0.5};
        PolarVector subhigh{3, k_test_arc_pos + k_test_arc_len*0.5};
        PolarVector objlow{2, k_test_arc_pos};
        PolarVector objhigh{2, k_test_arc_pos + k_test_arc_len};
        subject.anchor_low  = to_cartesian(sublow);
        subject.anchor_high = to_cartesian(subhigh);
        object.anchor_low  = to_cartesian(objlow);
        object.anchor_high = to_cartesian(objhigh);
        assert(has_valid_positions(subject) && has_valid_positions(object));
        update_for_possible_obstruction(subject, object);
        return test(are_very_close(subject.visibility, 0.75));
    });
    mark(suite).test([] {
        ImageEntry subject, object;
        object.opactity = 0.6;
        assert(completely_overlaps_source(object));
        subject.anchor_low = to_cartesian(PolarVector{3, k_test_arc_pos});
        subject.anchor_high = to_cartesian(PolarVector{3, k_test_arc_pos + k_test_arc_len});
        update_for_possible_obstruction(subject, object);
        return test(are_very_close(subject.visibility, 0.4));
    });
}
