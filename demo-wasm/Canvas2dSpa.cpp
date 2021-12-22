#include <emscripten.h>

#include "../demo-src/DemoDriver.hpp"

// in firefox this:
// privacy.file_unique_origin
// will need to be changed to false in order to run local files, otherwise I'll
// get a CORS error

namespace {

using std::round;

VectorI round(Vector r)
    { return VectorI{int(round(r.x)), int(round(r.y))}; } 

class CanvasDrawer final : public DrawInterface {
public:
    static CanvasDrawer & instance() {
        static CanvasDrawer inst;
        return inst;
    }
    
    void draw_string_center(const std::string &, Vector center) final;

    void draw_rectangle(const Rectangle &, const char * color) final;

    void draw_string_top_left(const std::string &, Vector top_left) final;

    Size2 draw_area() const final;
    
    void set_draw_area(Real width, Real height);
    
    void set_camera_position(Vector);
    
private:
    void draw_cone_
        (const Vector & source, const Vector & facing, Real distance, Real spread_angle) final;

    void draw_string(const std::string &, VectorI) const;

    CanvasDrawer() {}
    Size2 m_canvas_size;
    Vector m_camera_position;
};

DemoDriver & program_state() {
    static DemoDriver inst;
    return inst;
}

template <typename Func>
void do_try(Func && f) noexcept;

enum KeyIds { k_up, k_down, k_right, k_left, k_pause, k_frame_advance };

Key to_impl_key(int key_id) {
    switch (key_id) {
    case k_up           : return Key::up           ;
    case k_down         : return Key::down         ;
    case k_right        : return Key::right        ;
    case k_left         : return Key::left         ;
    case k_pause        : return Key::pause        ;
    case k_frame_advance: return Key::frame_advance;
    default             : return Key::none         ;
    
    }
}

} // end of <anonymous> namespace

// -------------------------- Calls to JavaScript ------------------------------

EM_JS(void, em_js_fill_rect, (int x, int y, int w, int h), {
    global_2d_context.fillRect(x, y, w, h);
});

EM_JS(void, em_js_set_fillStyle, (const char * fill_style), {
    global_2d_context.fillStyle = Module.UTF8ToString(fill_style);
});

EM_JS(void, em_js_set_strokeStyle, (const char * style_), {
    global_2d_context.strokeStyle = Module.UTF8ToString(style_);
});

EM_JS(void, em_js_set_textBaseline, (const char * style), {
    global_2d_context.textBaseline = Module.UTF8ToString(style);
});

EM_JS(int, em_js_get_text_width, (const char * string), {
    return Math.round(global_2d_context.measureText(Module.UTF8ToString(string)).width);
});

EM_JS(void, em_js_fillText, (const char * text, int x, int y), {
    global_2d_context.fillText(Module.UTF8ToString(text), x, y);
});

EM_JS(void, em_js_strokeText, (const char * text, int x, int y), {
    global_2d_context.strokeText(Module.UTF8ToString(text), x, y);
});

EM_JS(void, em_js_log_to_console, (const char * text), {
    console.log(Module.UTF8ToString(text));
});

EM_JS(void, em_js_beginPath, (), {
    global_2d_context.beginPath();
});

EM_JS(void, em_js_moveTo, (int x, int y), {
    global_2d_context.moveTo(x, y);
});

EM_JS(void, em_js_lineTo, (int x, int y), {
    global_2d_context.lineTo(x, y);
});

EM_JS(void, em_js_arcTo, (int a_x, int a_y, int b_x, int b_y, int radius), {
    global_2d_context.arcTo(a_x, a_y, b_x, b_y, radius);
});

EM_JS(void, em_js_fill, (), {
    global_2d_context.fill();
});

EM_JS(void, em_js_throw, (const char * name, const char * what_str), {
    console.log('exception! ' + Module.UTF8ToString(what_str));
    throw { name: Module.UTF8ToString(name), message: Module.UTF8ToString(what_str) };
});


// --------------------------- Calls from JavaScript ---------------------------

extern "C" {

EMSCRIPTEN_KEEPALIVE void js_glue_start() {
    do_try([&] {
        program_state().prepare_scenes();
    
        SceneOptions options;
        program_state().load_scene(options, /* third-scene */ 10);
    });
}

EMSCRIPTEN_KEEPALIVE void js_glue_load_scene(int scene_number, const char * scene_options) {
    do_try([&] { 
        SceneOptions options = load_options_from_string(scene_options);
        program_state().load_scene(options, scene_number);
    });
}

EMSCRIPTEN_KEEPALIVE void js_glue_on_canvas_resize(int width, int height) {
    do_try([&] { CanvasDrawer::instance().set_draw_area(Real(width), Real(height)); });
}

EMSCRIPTEN_KEEPALIVE void js_glue_on_update(double elapsed_time) {
    do_try([&] {
        program_state().on_update(elapsed_time);
    });
}

EMSCRIPTEN_KEEPALIVE void js_glue_render_field() {
    do_try([&] {
        CanvasDrawer::instance().set_camera_position(
            program_state().camera_center() 
            - cul::convert_to<Vector>(CanvasDrawer::instance().draw_area()*0.5));
        program_state().on_draw_field(CanvasDrawer::instance());
    });
}

EMSCRIPTEN_KEEPALIVE void js_glue_render_hud() {
    do_try([&] {
        CanvasDrawer::instance().set_camera_position(Vector{});
        program_state().on_draw_hud(CanvasDrawer::instance());
    });
}

EMSCRIPTEN_KEEPALIVE void js_glue_keydown(int key_id) {
    do_try([&]{ program_state().on_press(to_impl_key(key_id)); });
}

EMSCRIPTEN_KEEPALIVE void js_glue_keyup(int key_id) {
    do_try([&]{ program_state().on_release(to_impl_key(key_id)); });
}

EMSCRIPTEN_KEEPALIVE void js_glue_on_mouse_down(int mouse_x, int mouse_y) {}
EMSCRIPTEN_KEEPALIVE void js_glue_on_mouse_up  (int mouse_x, int mouse_y) {}
EMSCRIPTEN_KEEPALIVE void js_glue_on_mouse_move(int mouse_x, int mouse_y) {}

EMSCRIPTEN_KEEPALIVE const char * js_glue_get_scene_name(int num) {
    const char * rv = nullptr;
    do_try([&] {
        if (std::size_t(num) >= program_state().scene_names().size()) {
            rv = "";
            return;
        }
        rv = program_state().scene_names()[std::size_t(num)].c_str();
    });
    return rv;
}

EMSCRIPTEN_KEEPALIVE const char * js_describe_components(const char * component_name) {
    const char * rv = nullptr;
    do_try([&] {
        // string needs a place to live
        static std::string str;
        str = program_state().inquiry(component_name);
        rv = str.c_str();
    });
    return rv;
}

} // end of extern "C"

namespace {

constexpr const Real k_scale = 2.;

void CanvasDrawer::draw_string_center(const std::string & str, Vector center) {
    int width = em_js_get_text_width(str.c_str());
    auto r = round((center - m_camera_position)*k_scale);
    r.x -= width / 2;
    
    em_js_set_textBaseline("middle");
    draw_string(str, r);
}

void CanvasDrawer::draw_rectangle(const Rectangle & rect, const char * color) {
    em_js_set_fillStyle(color);
    em_js_fill_rect(
        int(round( (rect.left - m_camera_position.x)*k_scale )),
        int(round( (rect.top  - m_camera_position.y)*k_scale )),
        int(round( rect.width *k_scale )),
        int(round( rect.height*k_scale )));
}

void CanvasDrawer::draw_string_top_left(const std::string & str, Vector top_left) {
    em_js_set_textBaseline("top");
    draw_string(str, round((top_left - m_camera_position)*k_scale));
}

Size2 CanvasDrawer::draw_area() const { return m_canvas_size / k_scale; }

void CanvasDrawer::set_draw_area(Real width, Real height) {
    m_canvas_size = Size2{width, height};
}

void CanvasDrawer::set_camera_position(Vector r) {
    m_camera_position = r;
}

void CanvasDrawer::draw_cone_
    (const Vector & source, const Vector & facing, Real distance, Real spread_angle)
{
    auto low_pt  = find_cone_point(facing, distance, -spread_angle);
    auto high_pt = find_cone_point(facing, distance,  spread_angle);
    em_js_set_fillStyle("#7008");
    
    em_js_beginPath();
    em_js_moveTo(int(round(source.x )), int(round(source.y )));
    em_js_lineTo(int(round(low_pt.x )), int(round(low_pt.y )));
    em_js_arcTo (int(round(low_pt.x )), int(round(low_pt.y )),
                 int(round(high_pt.x)), int(round(high_pt.y)),
                 int(round(distance)) );
    em_js_lineTo(int(round(source.x)), int(round(source.y)));
    em_js_fill();
}

/* private */ void CanvasDrawer::draw_string(const std::string & str, VectorI r) const {
    em_js_set_fillStyle  ("#FFF");
    em_js_fillText       (str.c_str(), r.x, r.y);
    em_js_set_strokeStyle("#000");
    em_js_strokeText     (str.c_str(), r.x, r.y);
}

template <typename Func>
void do_try(Func && f) noexcept {
    try {
        f();
    } catch (std::invalid_argument & invarg) {
        em_js_throw("invalid_argument", invarg.what());
    } catch (std::runtime_error & rterr) {
        em_js_throw("runtime_error", rterr.what());
    } catch (...) {
        em_js_throw("unknown", "");
    }
}

} // end of <anonymous> namespace
