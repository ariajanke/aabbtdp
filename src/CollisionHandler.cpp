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

#include "CollisionHandler.hpp"
#ifdef MACRO_AABBTDP_LIBRARY_BUILD_FOR_PERSONAL_ECS_REFERENCE
#   include <ecs/ecs.hpp>
#endif

#include <iostream>
#include <thread>

#include <cassert>

namespace {

using namespace cul::exceptions_abbr;
using tdp::Vector, tdp::HitSide, tdp::Rectangle, tdp::FullEntry,
      tdp::EntryEntityRefMap, cul::is_real,
      cul::bottom_of, cul::right_of, cul::normalize, cul::magnitude,
      cul::set_top_left_of, cul::top_left_of, tdp::Tuple;

bool is_real(const Rectangle &);

} // end of <anonymous> namespace

namespace tdp {

void do_collision_work
    (EventHandler &, IterationBase &, const CollisionMatrix &,
     EventRecorder &, EntryMapView);

CollisionHandler::CollisionWorker::CollisionWorker
    (bool & b, EventRecorder & event_recorder,
     CollisionMatrix & col_matrix, EntryEntityRefMap & entries)
:
    m_was_called(b),
    m_event_recorder(event_recorder),
    m_col_matrix(col_matrix),
    m_entries(entries)
{}

void CollisionHandler::CollisionWorker::operator () (EventHandler & event_handler, IterationBase & iteration_method) {
    do_collision_work(event_handler, iteration_method, m_col_matrix,
                      m_event_recorder, View{m_entries.begin(), m_entries.end()});
    m_was_called = true;
}

void CollisionHandler::run(EventHandler & event_handler) {
    for (auto itr = m_entries.begin(); itr != m_entries.end();) {
        if (itr->second.entity) {
            ++itr;
        } else {
            itr = m_entries.erase(itr);
        }
    }

    bool was_called = false;
    CollisionWorker cw{was_called, m_event_recorder, m_col_matrix, m_entries};
    prepare_iteration(cw, event_handler);
    m_event_recorder.send_events(event_handler);
}

void CollisionHandler::update_entry(const Entry & entry) {
    if (collision_matrix().is_empty()) {
        throw InvArg("TdpHandlerComplete::update_entry: cannot update entries "
                     "with collision matrix not being set.");
    }
    if (entry.collision_layer == Entry::k_no_layer) {
        throw InvArg("TdpHandlerComplete::update_entry: entry must specify "
                     "its collision layer.");
    }
    if (!entry.entity) {
        throw InvArg("TdpHandlerComplete::update_entry: this entry needs to "
                     "have its entity reference set (member named \"entity\").");
    }
    if (   !::is_real(entry.bounds      ) || !is_real(entry.displacement )
        || !  is_real(entry.growth.width) || !is_real(entry.bounds.height))
    {
        throw InvArg("TdpHandlerComplete::update_entry: bounds, growth, and "
                     "displacement must all be in every field real numbers.");
    }
    if (entry.bounds.width < 0 || entry.bounds.height < 0) {
        throw InvArg("TdpHandlerComplete::update_entry: bounds size must be "
                     "non-negative real numbers.");
    }

    static constexpr const auto k_solid_has_growth_msg =
        "TdpHandlerComplete::update_entry: solid entry has growth, the entry "
        "must be either passive or trespass for every layer. (A behavior that "
        "is not implemented is the rationale for this error)";

    if (entry.growth != Size(0, 0)) {
        for (int x = 0; x != collision_matrix().width(); ++x) {
            if (collision_matrix()(x, entry.collision_layer) == InteractionClass::k_as_solid) {
                throw InvArg(k_solid_has_growth_msg);
            }
        }
    }
    static_cast<tdp::Entry &>(m_entries[entry.entity]) = entry;
}

void CollisionHandler::remove_entry(const Entity & entity)
    { (void)m_entries.erase(entity); }

void CollisionHandler::set_collision_matrix_(CollisionMatrix && matrix) {
    using VecI = CollisionMatrix::Vector;
    using namespace interaction_classes;
    if (matrix.width() != matrix.height()) {
        throw InvArg("TdpHandlerComplete::set_collision_matrix_: "
                     "collision matrix must be a square matrix.");
    }

    static auto make_pos_string = [](VecI r)
        { return std::to_string(r.x) + ", " + std::to_string(r.y); };

    for (VecI r; r != matrix.end_position(); r = matrix.next(r)) {
        if (r.x > r.y) continue;
        if (r.x == r.y) {
            if (matrix(r) != k_reflect) continue;
            throw InvArg("TdpHandlerComplete::set_collision_matrix_: "
                         "diagonal elements may not have a reflect value. ("
                         + make_pos_string(r) + ")");
        }
        if (matrix(r) != matrix(r.y, r.x)) {
            if (matrix(r) == k_reflect) {
                matrix(r) = matrix(r.y, r.x);
            } else if (matrix(r.y, r.x) == k_reflect) {
                matrix(r.y, r.x) = matrix(r);
            } else {
                throw InvArg("TdpHandlerComplete::set_collision_matrix_: "
                             "matrix is not symmetric along diagonal, mismatch "
                             "between: " + make_pos_string(r) + " "
                             + make_pos_string(VecI(r.y, r.x)));
            }
        } else {
            // both are "reflect"
            if (matrix(r) == k_reflect) {
                throw InvArg("TdpHandlerComplete::set_collision_matrix_: "
                             "reflect values may not be on the diagonal ("
                             + make_pos_string(r) + ").");
            }
        }
    }
    m_col_matrix = std::move(matrix);
}

// ----------------------------------------------------------------------------

class ColWorkImpl final : public IterationBase::SequenceInterface {
public:
    explicit ColWorkImpl
        (int p, EventRecorder & recorder, EventHandler & handler,
         const CollisionMatrix & col_mat):
        m_priority(p), m_event_recorder(recorder), m_event_handler(handler),
        m_col_matrix(col_mat) {}

    void prestep(FullEntry & entry) final;

    void step(FullEntry & entry, FullEntry & other_entry) final;

    void poststep(FullEntry & entry) final;

private:
    // is there a clean way to put priority up to the "boarder" phase?
    int m_priority;
    EventRecorder & m_event_recorder;
    EventHandler & m_event_handler;
    const CollisionMatrix & m_col_matrix;
};

void do_collision_work
    (EventHandler & event_handler, IterationBase & group_method,
     const CollisionMatrix & collision_matrix,
     EventRecorder & event_recorder, EntryMapView entries_view)
{
    for (auto & pair : entries_view) {
        // I guess reset before the frame, because for illustration purposes?
        pair.second.priority = k_default_priority;
    }

    // note: clean up must take place before populating the group method/container
    using std::get;
    static constexpr const int k_max_iterations = 128;
    int iteration = 0;
    for (bool should_check_more = true; should_check_more && iteration < k_max_iterations; ++iteration) {
        should_check_more = false;
        // We should only process entries of the current highest priority first
        // Then if we find we can push another entry, that pushed entry then
        // becomes an even higher priority.
        group_method.for_each(
            [&collision_matrix, &iteration, &should_check_more, &event_recorder]
            (FullEntry & entry, FullEntry & other_entry)
        {
            // only consider entry as a possible pusher if iteration == priority
            if (entry.priority != iteration) return;
            if (entry.displacement == Vector{} && entry.nudge == Vector{}) return;

            // is other_entry pushable?
            auto col_class = collision_matrix(entry.collision_layer, other_entry.collision_layer);
            if (col_class != InteractionClass::k_as_solid || !other_entry.pushable) return;

            // find push displacement
            auto gv = find_min_push_displacement(entry.bounds,
                displace(other_entry.bounds, other_entry.displacement + other_entry.nudge),
                entry.displacement + entry.nudge);
            if (get<Vector>(gv) == Vector{}) return;

            // The pushed entity should move with a higher priority
            other_entry.priority  = iteration + 1;
            other_entry.nudge    += get<Vector>(gv); // shoud I update board boundries here?!
            should_check_more     = true;
            event_recorder.emplace_event(other_entry.entity, entry.entity, CollisionEvent::k_push);
        });
    }

    absorb_nudges(entries_view); // shoud I update board boundries here?!

    // start finalizing
    assert(iteration > 0);
    for (; iteration; --iteration) {
        // process priority iteration - 1
        ColWorkImpl impl(iteration - 1, event_recorder, event_handler, collision_matrix);
        group_method.for_each_sequence(impl);
    }

    for (auto & pair : entries_view) {
        pair.second.first_appearance = false;
    }
}

// ----------------------------------------------------------------------------

void ColWorkImpl::prestep(FullEntry & entry) {
    // I'd like to pass priority checks to the board phase...
    // it could cull a lot
    if (entry.priority != m_priority) return;
    // Wall Collision
    Vector barrier = find_barrier_for_displacement
        (entry.displacement, entry.positive_barrier, entry.negative_barrier);
    auto [dis, hitside] = trim_displacement_for_barriers(entry.bounds, barrier, entry.displacement);
    entry.displacement = dis;
    if (hitside != HitSide()) {
        m_event_recorder.emplace_event(entry.entity, Entity{}, CollisionEvent::k_rigid);
    }
}

void ColWorkImpl::step(FullEntry & entry, FullEntry & other_entry) {
    if (entry.priority != m_priority) return;

    using namespace tdp::interaction_classes;
    switch (m_col_matrix(entry.collision_layer, other_entry.collision_layer)) {
    case k_as_solid: {
        auto [ndisplc, hitside] = trim_displacement(entry.bounds, other_entry.bounds, entry.displacement);
        if (hitside == HitSide()) return;
        entry.displacement = ndisplc;
        m_event_recorder.emplace_event(entry.entity, other_entry.entity, CollisionEvent::k_rigid);
        break;
    }
    case k_as_trespass: {
        bool first_appearance_overlap = entry.first_appearance && overlaps(entry.bounds, other_entry.bounds);
        bool regular_trespass         = trespass_occuring(entry.bounds, other_entry.bounds, entry.displacement);
        if (!first_appearance_overlap && !regular_trespass) return;
        // this is an "uh-oh" moment if we're reusing containers
        m_event_recorder.emplace_event(entry.entity, other_entry.entity, CollisionEvent::k_trespass);
        break;
    }
    default: break;
    }
}

void ColWorkImpl::poststep(FullEntry & entry) {
    if (entry.priority != m_priority) return;
    // fine with other bounds being finalized for trespass events
    set_top_left_of(entry.bounds, top_left_of(entry.bounds) + entry.displacement);
    if (entry.growth != Size()) {
        entry.bounds = grow(entry.bounds, entry.growth);
    }
    // this is fine here, not reusing containers at this point, unlike
    // while being in the for loop above
    //
    // how does entity become invalid?
    m_event_handler.finalize_entry(entry.entity, entry.bounds);
}

} // end of tdp namespace

namespace {

bool is_real(const Rectangle & rect) {
    using cul::is_real;
    return    is_real(rect.left ) && is_real(rect.top   )
           && is_real(rect.width) && is_real(rect.height);
}

} // end of <anonymous> namespace

// there are plans for a more accurate "trespass" detection
// which would involve ideas expressed here
#if 0

std::array<Vector, 4> get_rectangle_points(const Rectangle & rect) {
    auto tl = cul::top_left_of(rect);
    return std::array { tl, tl + Vector(rect.width, 0),
        tl + Vector(0, rect.height), tl + Vector(rect.width, rect.height)
    };
}

// this needs unit tests
Rectangle union_of(const Rectangle &, const Rectangle &);

struct GetTlBrRt {
    Real left, right, bottom, top;
};

GetTlBrRt get_top_left_and_bottom_right(const Rectangle &, const Rectangle &);

GetTlBrRt get_top_left_and_bottom_right(const Rectangle & lhs, const Rectangle & rhs) {
    using std::min, std::max;
    GetTlBrRt rv;
    rv.left   = min(lhs.left      , rhs.left      );
    rv.right  = max(right_of(lhs) , right_of(rhs) );
    rv.top    = min(lhs.top       , rhs.top       );
    rv.bottom = max(bottom_of(lhs), bottom_of(rhs));
    return rv;
}

// this needs unit tests too
// (and test with smaller sizes)
std::array<Vector, 8> convex_hull_of(const Rectangle &, const Rectangle &);

Rectangle union_of(const Rectangle & lhs, const Rectangle & rhs) {
    auto gv = get_top_left_and_bottom_right(lhs, rhs);
    return Rectangle(gv.left, gv.top, gv.right - gv.left, gv.bottom - gv.top);
}

std::array<Vector, 8> convex_hull_of(const Rectangle & lhs, const Rectangle & rhs) {
    using std::min, std::max;
    static auto ex_eq = [](Real a, Real b) { return std::equal_to<Real>()(a, b); };
    auto is_hull_point = [&lhs, &rhs] {
        auto ext = get_top_left_and_bottom_right(lhs, rhs);
        return [=] (Real x, Real y) {
            // these are all floating points!
            // but given the formulas, the values should come out as exactly
            // equal
            return    ex_eq(x, ext.left) || ex_eq(x, ext.right )
                   || ex_eq(y, ext.top ) || ex_eq(y, ext.bottom);
        };
    } ();

    std::array<Vector, 8> rv;
    auto witr = rv.begin();
    auto write_out = [&witr, &rv](Real x, Real y) {
        assert(witr != rv.end());
        *witr++ = Vector(x, y);
    };

    for (const auto & rect : { lhs, rhs }) {
        // if rect width  is zero, then right points should be skipped
        // if rect height is zero, then bottom points should be skipped
        // the point is added if either component is equal one extreme or the
        // other for its respective dimension

        // top left
        if (is_hull_point(rect.left, rect.top))
            write_out(rect.left, rect.top);

        // top right
        if (rect.width != 0 && is_hull_point(right_of(rect), rect.top)) {
            write_out(right_of(rect), rect.top);
        }
        // bottom left
        if (rect.height != 0 && is_hull_point(rect.left, bottom_of(rect))) {
            write_out(rect.left, bottom_of(rect));
        }
        // bottom right
        if (   rect.width != 0 && rect.height != 0
            && is_hull_point(right_of(rect), bottom_of(rect)))
        { write_out(right_of(rect), bottom_of(rect)); }
    }

    std::fill(witr, rv.end(), Vector(k_inf, k_inf));
    return rv;
}

bool trespass_occuring
    (const Rectangle & rect,
     const Rectangle & old_other, const Rectangle & new_other)
{
    using cul::overlaps, std::get, std::remove, std::make_tuple;
    // fast fail
    if (!overlaps(rect, union_of(old_other, new_other))) return false;

    // some fast succeeds
    if (old_other.width != 0 && old_other.height != 0) {
        if (overlaps(rect, old_other)) return true;
    }
    if (new_other.width != 0 && new_other.height != 0) {
        if (overlaps(rect, new_other)) return true;
    }

    // the much more laborous part
    auto gv  = convex_hull_of(old_other, new_other);
    auto itr = gv.begin();
    Vector pivot = *itr++;
    const Vector * last_point = &(*itr++);
    assert(is_real(*last_point) && is_real(pivot));
    // itr not end by size of returned array
    auto rect_pts = get_rectangle_points(rect);
    while (is_real(*itr)) {
        for (const auto & v : rect_pts) {
            if (is_inside_triangle(pivot, *last_point, *itr, v)) return true;
        }
        last_point = &*itr;
        ++itr;
        assert(itr != gv.end());
    }
    return false;
}

#endif
