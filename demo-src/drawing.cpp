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
using std::tuple;

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

const char * verify_ok_color_string(const char * s, const char * caller) {
    struct F final {
        static bool verify_length(const char * s) {
            const auto len = strlen(s);
            return len == 3 || len == 4 || len == 6 || len == 8;
        }
        static bool verify_hex_numbers(const char * s, bool b) {
            if (*s == 0) return b;
            return verify_hex_numbers(s + 1,
                b && (   (*s >= 'A' && *s <= 'F')
                      || (*s >= 'a' && *s <= 'f')
                      || (*s >= '0' && *s <= '9')));
        }
    };
    auto make_error = [caller] ()
        { return InvArg(std::string{caller} + ": requires a valid color string."); };
    if (*s != '#') throw make_error();
    assert(strlen(s) != 0);
    if (!F::verify_length(s + 1) || !F::verify_hex_numbers(s + 1, true))
        throw make_error();
    return s;
}

template <typename T, typename ToCString>
std::enable_if_t<!std::is_same_v<T, const char *>, T>
    verify_ok_color_string(T && s, const char * caller, ToCString && f)
{
    const char * cstr = f(s);
    (void)verify_ok_color_string(cstr, caller);
    return std::move(s);
}

template <int r0, int r1, int g0, int g1, int b0, int b1>
auto make_n_with_alpha(const char * s) {
    using std::array;
    return [s](char a0, char a1) {
        return verify_ok_color_string(
            array{'#', s[r0], s[r1], s[g0], s[g1], s[b0], s[b1], a0, a1, '\0'},
            "make_n_with_alpha",
            [](const auto & arr) { return arr.data(); });
    };
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

    static RGBA9String force_9char_rbga_string(const char * base_color) {
        if (*base_color != '#') throw InvArg("");
        const auto color_part = base_color + 1;
        auto three_with_alpha = make_n_with_alpha<0, 0, 1, 1, 2, 2>(color_part);
        auto six_with_alpha   = make_n_with_alpha<0, 1, 2, 3, 4, 5>(color_part);
        switch (strlen(color_part)) {
        case 3: return three_with_alpha('F', 'F');
        case 4: return three_with_alpha(base_color[3], base_color[3]);
        case 6: return six_with_alpha  ('F', 'F');
        case 8: return six_with_alpha  (base_color[6], base_color[7]);
        default: throw InvArg("Invalid color string");
        }
    }

    static RGBA9String color_with_alpha(const char * base, const char * alpha) {
        assert(strlen(base ) == 9);
        assert(strlen(alpha) == 2);
        return make_n_with_alpha<0, 1, 2, 3, 4, 5>(base + 1)(alpha[0], alpha[1]);
    };

    static auto alpha_portion_to_u8str(Real x) {
        assert(x >= 0 && x <= 1);
        static auto to_a = [](int i) -> char {
            const char rv = (i > 9) ? ((i - 10) + 'A') : (i + '0');
            assert((rv >= 'A' && rv <= 'F') || (rv >= '0' && rv <= '9'));
            return rv;
        };
        return std::array{ to_a(int(x*255) / 16), to_a(int(x*255) % 16), '\0' };
    };

    static Tuple<Rectangle, RGBA9String> make_float_rectangle
        (const Rectangle & base, Real age, const char * base_rgba9_color)
    {
        using cul::size_of, cul::top_left_of;
        assert(age >= 0 && age <= k_lifetime);
        const auto alpha     = 1 - age / k_lifetime;
        const auto alpha_str = alpha_portion_to_u8str(alpha);
        const auto color     = color_with_alpha(base_rgba9_color, alpha_str.data());
        const auto nrect     = displace(base, k_float_velocity*age);
        return make_tuple(nrect, color);
    }

    // there might be a resource effecient way to do this using std::function
    // but std::function needs to reuse the same buffer over and over again
    // which to be frank, I'm not sure about my current implementation

    class Stepper final {
    public:
        Stepper(int step, int step_count, Real float_time, const char * base_color,
                const Rectangle & base):
            m_step(step),
            m_step_count(step_count),
            m_float_time(float_time),
            m_base_color(base_color),
            m_base(base)
        {
            assert(strlen(base_color) == 9);
            assert(float_time <= k_cycle_max);
        }

        Stepper next() const {
            return Stepper{m_step + 1, m_step_count,
                m_float_time - k_spawn_delay, m_base_color,
                m_base};//displace(m_base, k_float_velocity*k_spawn_delay)};
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
        const char * const m_base_color;
        const Rectangle m_base;
    };

    template <typename ... Types>
    static auto do_step(Types && ... args)
        { return Stepper{std::forward<Types>(args)...}; }
};

/* protected */ void DrawSystemWithInterface::do_individual(const Entity & e) const {
    if (!e.has<Rectangle>()) return;
    auto & rect = e.get<Rectangle>();
    if (auto * color = e.ptr<Color>()) {
        draw_interface().draw_rectangle(rect, color->string);
        auto * frects = e.ptr<FloatRectangles>();
        auto float_time = frects ? frects->time : k_inf;
        if (cul::is_real(float_time)) {
            for (const auto & [rect, color] : make_drawable_float_rectangles(rect, color->string, frects->time)) {
                if (!cul::is_real(rect.left)) break;
                draw_interface().draw_rectangle(rect, color.data());
            }
        }
    }
    if (auto * dstring = e.ptr<DisplayString>()) {
        draw_interface().draw_string_center(dstring->value, center_of(rect));
    }
}

DrawableFloatRectangles make_drawable_float_rectangles
    (const Rectangle & base, const char * base_color, Real float_time)
{
    using F = MakeDrawableFloatRectanglesImpl;
    const auto ex_base_color = F::force_9char_rbga_string(base_color);
    const int step_count = int(std::min(float_time, F::k_lifetime) / F::k_spawn_delay) + 1;
    assert(step_count <= F::k_max_rectangles);
    struct G final {
    static void do_step(DrawableFloatRectangles & rv, const F::Stepper & stepper) {
        if (stepper.done()) return;
        auto [step, rect, color] = stepper();
        verify_ok_color_string(color.data(), "do_step");
        rv[step] = make_tuple(rect, color);
        do_step(rv, stepper.next());
    }
    };
    DrawableFloatRectangles rv;
    std::fill(rv.begin(), rv.end(), make_tuple(Rectangle{k_inf, k_inf, k_inf, k_inf}, RGBA9String{}));
    G::do_step(rv,
        F::do_step(0, step_count, std::fmod(float_time, F::k_cycle_max),
                   ex_base_color.data(), base));
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
