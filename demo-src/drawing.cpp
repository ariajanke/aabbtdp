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

#include "drawing.hpp"
#include <common/Vector2Util.hpp>

#include <cstring>

namespace {

using namespace cul::exceptions_abbr;
using std::tuple, std::make_tuple;

void draw_grid_line
    (DrawInterface & draw_intf, Rectangle bounds, Real thickness, Real spacing);

} // end of <anonymous> namespace

void draw_backround(DrawInterface & draw_interface, Vector camera_center, Size2 visible_area) {
    // "out of bounds"
    Rectangle center_rect{camera_center - convert_to<Vector>(visible_area*Real(0.5)), visible_area};
    draw_interface.draw_rectangle(center_rect, "#777");
    // "walkable" area
    draw_interface.draw_rectangle(
       Rectangle{0, 0, k_field_width, k_field_height},
       "#070");
    draw_grid_line(draw_interface, center_rect, 3, 100);
}

template <typename T>
cul::Rectangle<T> displace
    (const cul::Rectangle<T> & rect, const cul::Vector2<T> & r)
{ return cul::Rectangle<T>{rect.left + r.x, rect.top + r.y, rect.width, rect.height}; }

struct MakeDrawableFloatRectanglesImpl final {
    static constexpr const auto k_lifetime       = FloatRectangles::k_float_duration;
    static constexpr const auto k_spawn_delay    = FloatRectangles::k_spawn_delay;
    static constexpr const auto k_float_velocity = FloatRectangles::k_float_velocity;
    static constexpr const auto k_max_rectangles = FloatRectangles::k_max_rectangles;
    static constexpr const auto k_cycle_max      = FloatRectangles::k_cycle_max;

    static Tuple<Rectangle, ColorString> make_float_rectangle
        (const Rectangle & base, Real age, const ColorString & base_rgba_color)
    {
        using cul::size_of, cul::top_left_of;
        assert(age >= 0 && age <= k_lifetime);
        const auto alpha = 1 - age / k_lifetime;
        const auto color = base_rgba_color.alpha().portion(alpha);
        const auto nrect = displace(base, k_float_velocity*age);
        return make_tuple(nrect, color);
    }

    // there might be a resource effecient way to do this using std::function
    // but std::function needs to reuse the same buffer over and over again
    // which to be frank, I'm not sure about my current implementation

    class Stepper final {
    public:
        Stepper(int step, int step_count, Real float_time,
                const ColorString & base_color, const Rectangle & base):
            m_step(step),
            m_step_count(step_count),
            m_float_time(float_time),
            m_base_color(base_color),
            m_base(base)
        {
            assert(float_time <= k_cycle_max);
        }

        Stepper next() const {
            return Stepper{m_step + 1, m_step_count,
                m_float_time - k_spawn_delay, m_base_color, m_base};
        }

        auto operator () () const {
            assert(!done());
            // if an x is "born" every 2 days, but lives for 7 (whole days), what's
            // the age for any of them at n days?
            // 0 days -> 1 x; newborn
            // 3 days -> 2 x; 1 day old, 3 days
            // 5 days -> 3 x; 1, 3, and 5 days
            // 7 days -> 4 x, 1, 3, 5, and 7 days
            // 8 days -> 4 x, newborn, 2, 4, and 6 days
            // 9 days -> 4 x, 1, 3, 5, and 7 days
            // age of the first (oldest) rect given float time
            const auto rects_age = std::fmod(m_float_time, k_lifetime);
            return std::tuple_cat(Tuple<int>{m_step}, make_float_rectangle(m_base, rects_age, m_base_color));
        }

        bool done() const {
            return m_step == m_step_count || m_float_time <= 0;
        }

    private:
        const int m_step;
        const int m_step_count;
        const Real m_float_time;
        const ColorString & m_base_color;
        const Rectangle m_base;
    };

    template <typename ... Types>
    static auto do_step(Types && ... args)
        { return Stepper{std::forward<Types>(args)...}; }
};

/* protected */ void DrawSystemWithInterface::do_individual(const Entity & e) const {
    if (!e.has<Rectangle>()) return;
    auto & rect = e.get<Rectangle>();
    if (auto * color = e.ptr<ColorString>()) {
        draw_interface().draw_rectangle(rect, color->c_str());
        auto * frects = e.ptr<FloatRectangles>();
        auto float_time = frects ? frects->time : k_inf;
        if (cul::is_real(float_time)) {
            for (const auto & [rect, color] : make_drawable_float_rectangles(rect, *color, frects->time)) {
                if (!cul::is_real(rect.left)) break;
                draw_interface().draw_rectangle(rect, color.c_str());
            }
        }
    }
    if (auto * dstring = e.ptr<DisplayString>()) {
        draw_interface().draw_string_center(dstring->value, center_of(rect));
    }
}

DrawableFloatRectangles make_drawable_float_rectangles
    (const Rectangle & base, const ColorString & base_color, Real float_time)
{
    using F = MakeDrawableFloatRectanglesImpl;
    const int step_count = int(std::min(float_time, F::k_lifetime) / F::k_spawn_delay) + 1;
    assert(step_count <= F::k_max_rectangles);
    struct G final {
    static void do_step(DrawableFloatRectangles & rv, const F::Stepper & stepper) {
        if (stepper.done()) return;
        auto [step, rect, color] = stepper();
        rv[step] = make_tuple(rect, color);
        do_step(rv, stepper.next());
    }
    };
    DrawableFloatRectangles rv;
    std::fill(rv.begin(), rv.end(), make_tuple(Rectangle{k_inf, k_inf, k_inf, k_inf}, ColorString{}));
    assert(base_color.length() > 0);
    G::do_step(rv,
        F::do_step(0, step_count, std::fmod(float_time, F::k_cycle_max),
                   base_color, base));
    return rv;
}

namespace {

void draw_grid_line
    (DrawInterface & draw_intf, Rectangle bounds, Real thickness, Real spacing)
{
    assert(spacing > 0);
    assert(thickness >= 1);
    auto find_start = [](Real low, Real step) {
        auto rem = std::fmod(cul::magnitude(low), step);
        return low + ((low < 0) ? rem : -rem + step);
    };

    {
    int hsteps = int(std::ceil(bounds.width / spacing));
    Real x_pos = find_start(bounds.left, spacing);
    for (int i = 0; i != hsteps; ++i) {
        draw_intf.draw_rectangle(Rectangle{
            x_pos - thickness*Real(0.5), bounds.top, thickness, bounds.height
        }, "#FFF");

        x_pos += spacing;
    }
    }
    {
    int vsteps = int(std::ceil(bounds.height / spacing));
    Real y_pos = find_start(bounds.top , spacing);
    for (int i = 0; i != vsteps; ++i) {
        draw_intf.draw_rectangle(Rectangle{
            bounds.left, y_pos - thickness*Real(0.5), bounds.width, thickness
        }, "#FFF");
        y_pos += spacing;
    }
    }
}

} // end of <anonymous> namespace
