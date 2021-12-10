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

constexpr const auto k_up_key = sf::Keyboard::W;
constexpr const auto k_left_key = sf::Keyboard::A;
constexpr const auto k_down_key = sf::Keyboard::S;
constexpr const auto k_right_key = sf::Keyboard::D;
constexpr const auto k_pause_key = sf::Keyboard::Return;

Key to_impl_key(sf::Keyboard::Key k) {
    switch (k) {
    case k_up_key   : return Key::up;
    case k_left_key : return Key::left;
    case k_down_key : return Key::down;
    case k_right_key: return Key::right;
    case k_pause_key: return Key::pause;
    default         : return Key::none;
    }
}

uint8_t read_color_nibble(const char * cstr);
uint8_t read_color_byte(const char * cstr);

sf::Color to_sf_color(const char * cstring) {
    assert(cstring);
    if (*cstring != '#') {
        throw std::invalid_argument("Cannot convert to color");
    }
    ++cstring;
    switch (::strlen(cstring)) {
    case 3:
        return sf::Color(read_color_nibble(cstring + 0),
                         read_color_nibble(cstring + 1),
                         read_color_nibble(cstring + 2));
    case 6:
        return sf::Color(read_color_byte(cstring + 0),
                         read_color_byte(cstring + 1),
                         read_color_byte(cstring + 2));
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

    void draw_string(const std::string & string, Vector center) final {
        m_text_brush.set_text_center(convert_to<sf::Vector2f>(center), string);
        m_target.draw(m_text_brush);
    }

    void draw_rectangle(const Rectangle & rect, const char * color) final {
        DrawRectangle drect(rect.left, rect.top, rect.width, rect.height, to_sf_color(color));
        m_target.draw(drect);
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
    driver.set_draw_area(k_field_width, k_field_height);
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
                if (   event.type == sf::Event::KeyPressed
                    && event.key.code == sf::Keyboard::Escape)
                { window.close(); }
                auto fptr = event.type == sf::Event::KeyPressed ? (&DemoDriver::on_press) : (&DemoDriver::on_release);
                (driver.*fptr)(to_impl_key(event.key.code));
                break;
            }
            default: break;
            }
        }
        }
        window.clear();
        driver.on_update();
        DrawInterfaceImpl draw_intf{window};
        {
            auto view = window.getView();
            view.setCenter(convert_to<sf::Vector2f>(driver.camera_center()));
            window.setView(view);
        }
        driver.on_draw(draw_intf);
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
