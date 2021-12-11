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

namespace {

void draw_grid_line
    (DrawInterface & draw_intf, Rectangle bounds, Real thickness, Real spacing);

} // end of <anonymous> namespace

void draw_backround(DrawInterface & draw_interface, Vector camera_center, Size2 visible_area) {
    // "out of bounds"
    Rectangle center_rect{camera_center - convert_to<Vector>(visible_area*0.5), visible_area};
    draw_interface.draw_rectangle(center_rect, "#777");
    // "walkable" area
    draw_interface.draw_rectangle(
       Rectangle{0, 0, k_field_width, k_field_height},
       "#070");
    draw_grid_line(draw_interface, center_rect, 3, 100);
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
            x_pos - thickness*0.5, bounds.top, thickness, bounds.height
        }, "#FFF");

        x_pos += spacing;
    }
    }
    {
    int vsteps = int(std::ceil(bounds.height / spacing));
    Real y_pos = find_start(bounds.top , spacing);
    for (int i = 0; i != vsteps; ++i) {
        draw_intf.draw_rectangle(Rectangle{
            bounds.left, y_pos - thickness*0.5, bounds.width, thickness
        }, "#FFF");
        y_pos += spacing;
    }
    }
}

} // end of <anonymous> namespace
