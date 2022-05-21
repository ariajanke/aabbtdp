/****************************************************************************

    MIT License

    Copyright (c) 2022 Aria Janke

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

#pragma once

// nts: lets see how well my code is designed for expandsion! :3
//      (now that I'm doing this many months later!)

// "standard" headers to include
#include "helpers.hpp"
#include "CollisionHandler.hpp"
// also included as it's an expansion of sweep ideas
#include "physics-interval-sweep.hpp"

// 22-5-9 1033
// dev note: SweepContainer needs to be refactored
//           AABBs will need to be expanded the greater the angle
//           expansion maybe limited by chosing the rotate from another axis
//           (like from x for greater radians to y for fewer radians)

namespace tdp {

/// This is a rotation matrix
///
/// Since trig functions can be a little on the expensive side... I'm
/// micro-optimizing. :/
class RotationMatrix {
public:
    explicit RotationMatrix(Real t) noexcept:
        m_cos_t(std::cos(t)), m_sin_t(std::sin(t))
    {}

    constexpr Vector operator * (const Vector & r) const noexcept
        { return Vector{r.x*m_cos_t - r.y*m_sin_t, r.x*m_sin_t + r.y*m_cos_t}; }

private:
    Real m_cos_t, m_sin_t;
};

auto make_compute_board_boundries(const RotationMatrix & rotmat) {
    return [&] (const FullEntry &) -> BoardBoundries {
        // I need to "rotate" growth as well...
        //
        // and should I "anchor" the angle around x or y
        // (smaller angle -> less expandsion)
        (void)rotmat;
        return BoardBoundries{};
    };
}

class ArbitraryAxisSweepContainer final : public SweepContainer {
public:
    // yuck! side effects!
    int count_sweep() final;

private:
    // also maybe a yuck, side effects again...
    void for_each_sequence_(SequenceInterface &) final;
#   if 0
    // mmm... very familar functions, maybe we should refactor across?
    static Real low_i (const FullEntry & fe) { return board_i(fe).low_left().left ; }
    static Real low_j (const FullEntry & fe) { return board_j(fe).low_left().left ; }
    static Real high_i(const FullEntry & fe) { return board_i(fe).low_left().right; }
    static Real high_j(const FullEntry & fe) { return board_j(fe).low_left().right; }

    struct Interval final {
        constexpr Interval(Real l_, Real r_): left(l_), right(r_) {}
        constexpr Interval low_left() const {
            const bool left_is_low = left < right;
            return Interval{
                ( left_is_low)*left + (!left_is_low)*right,
                (!left_is_low)*left + ( left_is_low)*right
            };
        }
        Real left, right;
    };

    static Interval board_i(const FullEntry & fe)
        { return Interval{fe.board_bounds.low_x, fe.board_bounds.high_x}; }

    static Interval board_y(const FullEntry & fe)
        { return Interval{fe.board_bounds.low_y, fe.board_bounds.high_y}; }
#   endif
    RotationMatrix m_rot_mat;
};

class ArbitrarySweepPhysicsHandlerImpl final :
    public CollisionHandler, public ArbitrarySweepPhysicsHandler
{
public:

    void set_axis_angle(Real new_angle) final;

private:
    void prepare_iteration(CollisionWorker &, EventHandler &) final;

    void find_overlaps_(const Rectangle &, const OverlapInquiry &) const final;

    RotationMatrix m_rot_mat;
};

} // end of tdp namespace
