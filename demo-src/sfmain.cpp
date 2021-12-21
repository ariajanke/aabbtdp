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

// a couple of abstractions:
// one for the driver
// and one for the menu
// we don't have to start with a menu though
// I want as minimal of an interface for SFML as possible

#include "DemoDriver.hpp"

#include <SFML/Graphics/RenderWindow.hpp>
#include <SFML/Window/Event.hpp>
#include <SFML/System/Sleep.hpp>

#include <common/sf/DrawText.hpp>
#include <common/sf/DrawRectangle.hpp>

#include <cmath>
#include <cstring>

using cul::convert_to, cul::BitmapFont, cul::SfBitmapFont, cul::DrawRectangle, cul::DrawText;

constexpr const int k_window_height = 700;
constexpr const int k_window_width  = (k_window_height * 11) / 6;
constexpr const Real k_frame_time   = 1. / 60.;

constexpr const auto k_up_key        = sf::Keyboard::W;
constexpr const auto k_left_key      = sf::Keyboard::A;
constexpr const auto k_down_key      = sf::Keyboard::S;
constexpr const auto k_right_key     = sf::Keyboard::D;
constexpr const auto k_frame_advance = sf::Keyboard::Q;
constexpr const auto k_pause_key     = sf::Keyboard::Return;

sf::Vertex to_vertex(Vector r, sf::Color color)
    { return sf::Vertex{ convert_to<sf::Vector2f>(r), color }; }

Key to_impl_key(sf::Keyboard::Key k) {
    switch (k) {
    case k_up_key       : return Key::up;
    case k_left_key     : return Key::left;
    case k_down_key     : return Key::down;
    case k_right_key    : return Key::right;
    case k_pause_key    : return Key::pause;
    case k_frame_advance: return Key::frame_advance;
    default             : return Key::none;
    }
}

uint8_t read_color_nibble(const char * cstr);
uint8_t read_color_byte(const char * cstr);

sf::Color to_sf_color(const char * cstring) {
    assert(cstring);

    static auto verify_hash = [](const char * cstring) {
        if (*cstring != '#') {
            throw std::invalid_argument("Cannot convert to color");
        }
        return cstring + 1;
    };

    static auto case_3 = [](const char * cstring) {
        return sf::Color(read_color_nibble(cstring + 0),
                         read_color_nibble(cstring + 1),
                         read_color_nibble(cstring + 2));
    };

    static auto case_6 = [](const char * cstring) {
        return sf::Color(read_color_byte(cstring + 0),
                         read_color_byte(cstring + 2),
                         read_color_byte(cstring + 4));
    };

    static auto with_alpha = [](sf::Color color, uint8_t alpha)
        { return sf::Color(color.r, color.g, color.b, alpha); };

    switch (::strlen(cstring = verify_hash(cstring))) {
    case 3: return case_3(cstring);
    case 4: return with_alpha(case_3(cstring), read_color_nibble(cstring + 3));
    case 6: return case_6(cstring);
    case 8: return with_alpha(case_6(cstring), read_color_byte(cstring + 6));
    default:
        throw std::invalid_argument("");
    }
}

class DrawInterfaceImpl final : public DrawInterface {
public:
    DrawInterfaceImpl(sf::RenderTarget & target):
        m_target(target)
    {
        m_text_brush.assign_font(m_font);
    }

    void draw_string_center(const std::string & string, Vector center) final {
        m_text_brush.set_text_center(convert_to<sf::Vector2f>(center), string);
        m_target.draw(m_text_brush);
    }

    void draw_string_top_left(const std::string & string, Vector top_left) final {
        m_text_brush.set_text_top_left(convert_to<sf::Vector2f>(top_left), string);
        m_target.draw(m_text_brush);
    }

    void draw_rectangle(const Rectangle & rect, const char * color) final {
        DrawRectangle drect(rect.left, rect.top, rect.width, rect.height, to_sf_color(color));
        m_target.draw(drect);
    }

    void draw_cone_
        (const Vector & source, const Vector & facing, Real distance, Real spread_angle) final
    {
        int segments = int(std::floor(spread_angle*distance / 40)) + 1;

        Real angl_delta = spread_angle / segments;
        Real t = -spread_angle;
        for (int i = 0; i != segments*2; ++i) {
            auto color = sf::Color(180, 0, 0, 200);
            auto next_t = std::min(t + angl_delta, spread_angle);
            std::array<sf::Vertex, 3> triangle = {
                to_vertex(source, color),
                to_vertex(source + find_cone_point(facing, distance,      t), color),
                to_vertex(source + find_cone_point(facing, distance, next_t), color)
            };
            t = next_t;
            m_target.draw(triangle.data(), triangle.size(), sf::PrimitiveType::Triangles);

        }
    }

    Size2 draw_area() const final
        { return convert_to<Size2>(m_target.getView().getSize()); }

private:
    sf::RenderTarget & m_target;
    const SfBitmapFont & m_font = SfBitmapFont::load_builtin_font(BitmapFont::k_8x8_highlighted_font);
    DrawText m_text_brush;
};

int main() {
    DemoDriver driver;
    driver.prepare_scenes();

    {
    SceneOptions options;
    driver.load_scene(options, /* first-scene */ 3);

    }

    sf::RenderWindow window(sf::VideoMode(k_window_width, k_window_height), " ");

    window.setFramerateLimit(60u);
    if (true) {
    auto view = window.getView();
    view.setSize(k_window_width / 2, k_window_height / 2);
    view.setCenter(0, 0);
    window.setView(view);
    }
    while (window.isOpen()) {
        {
        sf::Event event;
        while (window.pollEvent(event)) {
            switch (event.type) {
            case sf::Event::KeyPressed:
            case sf::Event::KeyReleased: {
                auto fptr = event.type == sf::Event::KeyPressed ? (&DemoDriver::on_press) : (&DemoDriver::on_release);
                (driver.*fptr)(to_impl_key(event.key.code));
            }
            default: break;
            }
            switch (event.type) {
            case sf::Event::KeyPressed:
                if (event.key.code == sf::Keyboard::Escape)
                    window.close();
                break;
            case sf::Event::KeyReleased: {
                if (event.key.code == sf::Keyboard::Num0) {
                    SceneOptions options;
                    driver.load_scene(options, 3);
                }
                break;
            }
            default: break;
            }
        }
        }
        window.clear();
        driver.on_update(k_frame_time);
        {
            DrawInterfaceImpl draw_intf{window};
            auto view = window.getView();
            view.setCenter(convert_to<sf::Vector2f>(driver.camera_center()));
            window.setView(view);

            driver.on_draw_field(draw_intf);
            view.setCenter(view.getSize().x / 2, view.getSize().y / 2);
            window.setView(view);
            driver.on_draw_hud(draw_intf);
        }

        window.display();
        sf::sleep(sf::microseconds(std::int64_t(std::round(1'000'000.0 * k_frame_time))));
    }
}

uint8_t get_val(char c);
uint8_t read_color_nibble(const char * cstr) {
    assert(cstr);
    assert(*cstr);
    return get_val(*cstr) | (get_val(*cstr) << 4);
}

uint8_t read_color_byte(const char * cstr) {
    assert(cstr);
    assert(cstr[0] && cstr[1]);

    return get_val(cstr[1]) | (get_val(cstr[0]) << 4);
}

uint8_t get_val(char c) {
    /**/ if (c >= '0' && c <= '9') { return c - '0'; }
    else if (c >= 'a' && c <= 'f') { return (c - 'a') + 10; }
    else if (c >= 'A' && c <= 'F') { return (c - 'A') + 10; }
    else { throw std::invalid_argument(""); }
}
