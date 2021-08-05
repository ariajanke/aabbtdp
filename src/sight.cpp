#include "sight-detail.hpp"
#include "helpers.hpp"

#include <numeric>

#include <cassert>

namespace { // ----------------------------------------------------------------

using tdp::detail::SightingComplete, tdp::detail::ImageEntry,
      tdp::detail::PolarVector, tdp::detail::ImageSpatialMapFactory,
      tdp::detail::AngleGetters, tdp::Sighting, tdp::Real, tdp::Vector,
      tdp::Rectangle;
using Percept = SightingComplete::Percept;
using Entry = SightingComplete::Entry;
using UnitTestFunctions = tdp::detail::SightingUnitTestFunctions;
using MapBase = ImageSpatialMapFactory::MapBase;
template <typename T>
/* portmanteau of "varriable array" like "vary" */ using Varray = std::vector<T>;
template <typename ... Types>
using Tuple = std::tuple<Types...>;

using cul::rotate_vector, cul::directed_angle_between, cul::top_left_of,
      cul::is_contained_in, cul::cross, cul::find_intersection,
      cul::get_no_solution_sentinel, cul::magnitude,
      std::min_element, std::max_element, std::make_tuple;//, tdp::detail::get_div;
using namespace cul::exceptions_abbr;

static constexpr const Real k_pi = cul::k_pi_for_type<Real>;

inline Vector make_anchor() { return Vector{std::cos(0), std::sin(0)}; }
inline bool theta_range_ok(const PolarVector & v)
    { return v.theta >= -k_pi && v.theta <= k_pi; }

// how do I unit test these things?

// mmm... what would be a better temporary name for "vector"

// converts entries into fully unobstructed images
void make_images(Vector source, const Varray<Entry> &, Varray<ImageEntry> &);

void update_for_possible_obstruction(ImageEntry &, const ImageEntry &);

Percept convert(Vector source, const ImageEntry &);

// returns [0 1]
// for each arc segment, beg to last implies an interval that is [beg last]
// rather than the usual [beg end)
//
// the subject is the segment that we're finding the portion of that's
// overlapped by the object
Real find_portion_overlapped
    (const PolarVector & subject_beg, const PolarVector & subject_last,
     const PolarVector & object_beg, const PolarVector & object_last);

// I'm not sure if I *really* want to move this up or not :/
ImageEntry make_image(Vector source, const Entry &);

} // end of <anonymous> namespace ---------------------------------------------

/* static */ std::unique_ptr<Sighting> Sighting::make_instance()
    { return std::make_unique<SightingComplete>(); }

// ----------------------------------------------------------------------------

Real AngleGetters::get_low(const ImageEntry & entry) const {
    // implementation assumption: the arc length of a cartesian line segment
    // cannot be greater than pi
    return PolarVector{entry.anchor_low}.theta;
}

Real AngleGetters::get_high(const ImageEntry & entry) const {
    return PolarVector{entry.anchor_high}.theta;
}

Real AngleGetters::domain_min() const { return -k_pi; }

Real AngleGetters::domain_max() const { return k_pi; }

// ----------------------------------------------------------------------------

MapBase & ImageSpatialMapFactory::choose_map_for
    (SetElIterator beg, SetElIterator end, int depth)
{
    if (depth > k_depth_max || end - beg < k_fork_thershold) return m_flat;
    auto div    = get_division_for<ImageEntry>(beg, end, AngleGetters{});
    auto counts = get_counts<ImageEntry, AngleGetters>(div, beg, end);
    if (too_many_shared(counts)) return m_flat;
    if (!m_part_ptr) {
        m_part_ptr = std::make_unique<PartMap>();
    }
    m_part_ptr->set_division(div);
    return *m_part_ptr;
}

// ----------------------------------------------------------------------------

void SightingComplete::add_entry(const Entry & entry) {
    m_preentries.push_back(entry);
}

const std::vector<Percept> & SightingComplete::run(Vector source) {
    m_percepts.clear();
    make_images(source, m_preentries, m_entries);
    m_spatial_map.set_elements(m_entries.begin(), m_entries.end());

    for (auto & entry : m_entries) {
        m_percepts.emplace_back(find_percept_of(source, entry));
    }

    //m_percepts.erase(std::remove_if(m_percepts.begin(), m_percepts.end(), [](const Percept & per) { return per.visibility == 0; }),
    //                 m_percepts.end());

    // on exit stuff
    m_entries.clear();
    m_preentries.clear();
    return m_percepts;
}

std::vector<std::tuple<Vector, Vector>>
    SightingComplete::make_image_lines(Vector source, std::vector<std::tuple<Vector, Vector>> && rv) const
{
    rv.clear();
    for (const auto & entry : m_preentries) {
        auto image = make_image(source, entry);
        rv.emplace_back(image.anchor_low, image.anchor_high);
    }

    return std::move(rv);
}

/* private */ Percept SightingComplete::find_percept_of
    (Vector source, const ImageEntry & original_image) const
{
    using ElementContainer = RadialSpatialMap::ElementContainer;
    /*static thread_local */ElementContainer temp_cont;
    temp_cont = m_spatial_map.collect_candidates(original_image, std::move(temp_cont));
    auto image_copy = original_image;
    for (auto * other_entry : temp_cont) {
        if (other_entry == &original_image) continue;
        update_for_possible_obstruction(image_copy, *other_entry);
    }
    return convert(source, image_copy);
}

namespace { // ----------------------------------------------------------------

// Images are created relative to the source (point of observation)
ImageEntry make_image(Vector source, const Entry &);

// this one assumes no intersection
// and therefore assumes one is entirely infront/behind the other
// (except the edge case that either two end points converge)
Real portion_infront(const PolarVector & obj_beg, const PolarVector & obj_last,
                     const PolarVector & sub_beg, const PolarVector & sub_last);

void make_images(Vector observ, const Varray<Entry> & source, Varray<ImageEntry> & dest) {
    dest.clear();
    dest.reserve(source.size());
    using namespace std::placeholders;
    std::transform(source.begin(), source.end(), std::back_inserter(dest),
                   std::bind(make_image, observ, _1));
}

void update_for_possible_obstruction
    (ImageEntry & image, const ImageEntry & other)
{
    // the other image's visibility has no effect on this image's visibility
    Real overlap_por;
    if (completely_overlaps_source(other)) {
        overlap_por = 1;
    } else {
        overlap_por = find_portion_overlapped(
          PolarVector{image.anchor_low}, PolarVector{image.anchor_high},
          PolarVector{other.anchor_low}, PolarVector{other.anchor_high});
    }
    // 0 overlap -> no effect
    // 1 overlap -> maximum effect
    // 0 opacity -> no effect
    // 1 opacity -> whipes out image by the overlap proportion
    // below: 1 -> no effect, 0 -> whipes out image completely
    image.visibility *= (1 - overlap_por*other.opactity);
}

// percept has the target position, so we bring that back into the global
// frame of reference
Percept convert(Vector source, const ImageEntry & image) {
    // image's visibility is affected by its own opacity (or lack thereof)
    Percept percept;
    percept.entity     = image.entity;
    percept.visibility = image.visibility*image.opactity;
    percept.target     = (image.anchor_low + image.anchor_high)*0.5 + source;
    return percept;
}

Real find_portion_overlapped
    (const PolarVector & sub_beg, const PolarVector & sub_last,
     const PolarVector & obj_beg, const PolarVector & obj_last)
{
    assert(   theta_range_ok(sub_beg) && theta_range_ok(sub_last)
           && theta_range_ok(obj_beg) && theta_range_ok(obj_last));

    using std::max, std::min;
    static auto is_completely_behind = []
        (const PolarVector & obj_beg, const PolarVector & obj_last,
         const PolarVector & sub_beg, const PolarVector & sub_last)
    { return min(obj_beg.r, obj_last.r) > max(sub_beg.r, sub_last.r); };

    static auto assert_correct_rv = [](Real rv) {
        assert(rv >= 0 && rv <= 1);
        return rv;
    };

    // object behind subject -> no overlap possible
    if (is_completely_behind(obj_beg, obj_last, sub_beg, sub_last))
        return 0;

    // object obstructs entirety of subject
    auto sub_beg_cart  = to_cartesian(sub_beg);
    auto sub_last_cart = to_cartesian(sub_last);
    auto intx_cart     = find_intersection(
        sub_beg_cart, sub_last_cart, to_cartesian(obj_beg), to_cartesian(obj_last));

    if (intx_cart == get_no_solution_sentinel<Vector>()) {
        // no intersection
        return portion_infront(obj_beg, obj_last, sub_beg, sub_last);
    }

    // if there is an intersection we split and process each seperately
    auto len = magnitude(sub_last_cart - sub_beg_cart);
    auto beg_to_intx_part  = magnitude(sub_beg_cart - intx_cart ) / len;
    auto intx_to_last_part = magnitude(intx_cart - sub_last_cart) / len;
    PolarVector intx{intx_cart};
    return assert_correct_rv(
          portion_infront(obj_beg,  intx, sub_beg, intx )*beg_to_intx_part
        + portion_infront(intx, obj_last, intx, sub_last)*intx_to_last_part);
}

// ----------------------------------------------------------------------------

template <typename T>
cul::Vector2<T> top_right_of(const cul::Rectangle<T> &);

template <typename T>
cul::Vector2<T> bottom_right_of(const cul::Rectangle<T> &);

template <typename T>
cul::Vector2<T> bottom_left_of(const cul::Rectangle<T> &);

Real directed_angle_between_(const Vector & a, const Vector & b);

auto bind_lesser_angle_from(Vector r) {
    return [r](const Vector & a, const Vector & b)
        { return directed_angle_between_(r, a) < directed_angle_between_(r, b); };
}

bool crosses_anti_anchor_line(const Vector & obsver, const Rectangle &);

ImageEntry make_image(Vector source, const Entry & entry) {
    ImageEntry rv;
    rv.entity   = entry.entity;
    rv.opactity = entry.opacity;
    if (is_contained_in(source, entry.bounds)) {
        rv.anchor_low = rv.anchor_high = Vector{};
        assert(completely_overlaps_source(rv));
        return rv;
    }

    Vector chosen_anchor;
    if (crosses_anti_anchor_line(source, entry.bounds)) {
        chosen_anchor = -make_anchor();
    } else {
        chosen_anchor = make_anchor();
    }

    std::array candidate_rays = {
        top_left_of    (entry.bounds) - source,
        top_right_of   (entry.bounds) - source,
        bottom_left_of (entry.bounds) - source,
        bottom_right_of(entry.bounds) - source
    };

    // this isn't good enough... or is it?
    // this is why I should unit test
    auto crbeg = candidate_rays.begin();
    auto crend = candidate_rays.end();

    auto lesser_angle_from_anchor = bind_lesser_angle_from(chosen_anchor);
    rv.anchor_low  = *max_element(crbeg, crend, lesser_angle_from_anchor);
    rv.anchor_high = *min_element(crbeg, crend, lesser_angle_from_anchor);
    return rv;
}

// potion of object infront of subject (assume no intersection)
Real portion_infront(const PolarVector & obj_beg, const PolarVector & obj_last,
                     const PolarVector & sub_beg, const PolarVector & sub_last)
{
    // lambda in an assert should be zerod out in release
    assert(   theta_range_ok(sub_beg) && theta_range_ok(sub_last)
           && theta_range_ok(obj_beg) && theta_range_ok(obj_last));
    assert([&] {
        if (obj_beg == sub_beg || obj_last == sub_last) return true;
        return find_intersection(
                to_cartesian(obj_beg), to_cartesian(obj_last),
                to_cartesian(sub_beg), to_cartesian(sub_last)
            ) == get_no_solution_sentinel<Vector>();
    } ());

    // edge cases where object is behind
    if (obj_beg == sub_beg) {
        if (obj_last.r > sub_last.r) return 0;
    } else if (obj_beg.r > sub_beg.r) {
        return 0;
    }

    static auto get_high_theta = []
        (const PolarVector & beg, const PolarVector & last)
    { return last.theta < beg.theta ? last.theta + k_pi*2 : last.theta; };

    static auto portion_infront_impl = []
        (Real obj_low, Real obj_high, Real sub_low, Real sub_high)
    {
        assert(obj_low <= obj_high && sub_low <= sub_high);
        static auto verify_with_01 = [](Real r) {
            assert(r >= 0 && r <= 1);
            return r;
        };
        static auto handle_div = [](Real denom, Real num) -> Real {
            // single point image? then it is covered completely
            // (note we've already determined that we're in the object interval)
            if (num == 0) return 1;
            return verify_with_01(denom / num);
        };
        bool obj_high_inside = obj_high >= sub_low && obj_high <= sub_high;
        if (obj_low >= sub_low && obj_low <= sub_high) {
            if (obj_high_inside) {
                // obj with sub
                return handle_div(obj_high - obj_low, sub_high - sub_low);
            } else {
                // obj low butts into sub
                return handle_div(sub_high - obj_low, sub_high - sub_low);
            }
        }
        if (obj_high_inside) {
            return handle_div(obj_high - sub_low, sub_high - sub_low);
        }
        return Real(0);
    };

    // object must be infront of subject, now, how much is infront?
    // total non overlap must be accounted for
    return portion_infront_impl(obj_beg.theta, get_high_theta(obj_beg, obj_last),
                                sub_beg.theta, get_high_theta(sub_beg, sub_last));
}

// ----------------------------------------------------------------------------

template <typename T>
cul::Vector2<T> top_right_of(const cul::Rectangle<T> & rect)
    { return cul::Vector2<T>(rect.left + rect.width, rect.top); }

template <typename T>
cul::Vector2<T> bottom_right_of(const cul::Rectangle<T> & rect)
    { return cul::Vector2<T>(rect.left + rect.width, rect.top + rect.height); }

template <typename T>
cul::Vector2<T> bottom_left_of(const cul::Rectangle<T> & rect)
    { return cul::Vector2<T>(rect.left, rect.top + rect.height); }

Real directed_angle_between_(const Vector & a, const Vector & b) {
    auto rv = directed_angle_between(a, b);
    /**/ if (rv < -k_pi) rv += 2*k_pi;
    else if (rv >  k_pi) rv -= 2*k_pi;
    assert(rv >= -k_pi && rv <= k_pi);
    return rv;
}

bool crosses_anti_anchor_line(const Vector & obsver, const Rectangle & rect) {
    // the correctness of this implementation depends on the following
    // assertion
    assert(   std::equal_to<Real>{}(make_anchor().x, 1.)
           && std::equal_to<Real>{}(make_anchor().y, 0.));
    return    cul::right_of(rect) < obsver.x
           && obsver.y >= rect.top && obsver.y <= cul::bottom_of(rect);
}

} // end of <anonymous> namespace

// ----------------------------------------------------------------------------
// I place this on the bottom to make sure I can reach all functions
// and initialize my little functions structure, so I can unit test my "hidden"
// functions
//
// (Seems like the least intrusive approach)

/* static */ UnitTestFunctions UnitTestFunctions::make_instance() {
    UnitTestFunctions rv;
    rv.find_portion_overlapped         = ::find_portion_overlapped;
    rv.make_image                      = ::make_image;
    rv.crosses_anti_anchor_line        = ::crosses_anti_anchor_line;
    rv.update_for_possible_obstruction = ::update_for_possible_obstruction;
    rv.make_anchor                     = ::make_anchor;
    return rv;
}
