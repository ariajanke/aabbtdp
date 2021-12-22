/* jshint esversion: 6 */

let program_state;

(() => {

"use strict";

const k_frame_rate  = 45;
const k_event_types = Object.freeze([
    'on_keyup', 'on_keydown', 'on_mousedown', 'on_mouseup', 'on_mousemove' ]);

const load_module_methods = module => Object.freeze({
    get_scene_name      : module.cwrap('js_glue_get_scene_name'  , 'string', ['number']),
    load_scene          : module.cwrap('js_glue_load_scene'      , null    , ['number', 'string']),
    time_update         : module.cwrap('js_glue_on_update'       , null    , ['number']),
    render_field        : module.cwrap('js_glue_render_field'    , null    , null),
    render_hud          : module.cwrap('js_glue_render_hud'      , null    , null),
    keyup               : module.cwrap('js_glue_keyup'           , null    , ['number']),
    keydown             : module.cwrap('js_glue_keydown'         , null    , ['number']),
    mouseclick          : module.cwrap('js_glue_on_mouse_down'   , null    , ['number', 'number']),
    mouseup             : module.cwrap('js_glue_on_mouse_up'     , null    , ['number', 'number']),
    mousemove           : module.cwrap('js_glue_on_mouse_move'   , null    , ['number', 'number']),
    start               : module.cwrap('js_glue_start'           , null    , null),
    describe_components : module.cwrap('js_describe_components'  , 'string', ['string']),
    canvas_resize       : module.cwrap('js_glue_on_canvas_resize', null    , ['number', 'number'])
});

const verify_dictionary_has_events = dict => {
    k_event_types.forEach(el => {
        if (dict[el] !== undefined) return;
        throw 'Dictionary must define all event types';
    });
    return dict;
};

const change_class_for = (element, old_class, new_class) => {
    if (element.hasClass(old_class)) {
        element.removeClass(old_class);
        element.addClass(new_class);
    }
};

const get_scene_options = options_sel => {
    const ills = (options_sel.find('input[name=illustrated]').val() === 'true' ? '' : 'no-')
                 + 'illustrate-algorithm';
    // can be used directly
    return options_sel.find('input[name=iteration-algorithm]:checked').val()
           + ',' + ills;
};

const make_program_state = () => {
    let me = {};
    
    // let's try and aim for "as functional as possible"
    // since we can't be pure AND have interesting things happen...
    let m_do_update;
    let m_needs_to_pause = false;
    let m_events;
    let m_model;
    let m_issue_resize;

    // ----------------------- hidden functions/closures -----------------------

    const prepare_scenes_menu = model => {
        // lets try to do more functional style of programming here c:
        const load_scene_names = model => {
            const get_at_index = (n, arr) => {
                // this call is *not* so pure
                const name = model.get_scene_name(n);
                if (name === '') return arr;
                
                return [name].concat( get_at_index(n + 1, arr) );
            };
            return get_at_index(0, []);
        };
        
        const menu_html = load_scene_names(model)
            .map((name, idx) => '<a class="scene-link" href="#" scene-number="' + idx + '">' + name + '</a><br>')
            .reduce((bulk, el) => bulk + el);
        
        return [menu_html, function () {
            const opts = get_scene_options($('#other-scene-options'));
            model.load_scene(parseInt($(this).attr('scene-number')), opts);
            $(this).blur();
            $('#options-menu').removeClass('slide-in').addClass('slide-out');
            me.play();
        }];
    };
    
    const make_update_routine = model => {
        return () => {
            console.log('game started');
            let old_time = new Date();
            const interval = setInterval(() => {
                if (m_needs_to_pause) {
                    clearInterval(interval);
                    return;
                }
                const new_time = new Date();
                model.time_update((new_time - old_time) / 1000);
                old_time = new_time;
                model.render_field();
                model.render_hud();
            }, 1000 / k_frame_rate);
        };
    };
    
    const make_event_methods = (model, pause_overlay) => {
        // I need to integrate menus...
        
        const make_act    = (n) => (f) => f(n); 
        const key_func_table = Object.freeze({
            'w' : make_act(0), 'a' : make_act(3), 's' : make_act(1),
            'd' : make_act(2), 'p' : make_act(4), 'o' : make_act(5),
            // pausing is pretty special
            'Enter' : state_act => {
                if (state_act !== model.keyup) return;
                (pause_overlay.hasClass('overlay-fade-in') ? me.play : me.pause)();
            }
        });
        
        return verify_dictionary_has_events(Object.freeze({
            on_keyup     : event => (key_func_table[event.key] || (() => {}))(model.keyup  ),
            on_keydown   : event => (key_func_table[event.key] || (() => {}))(model.keydown),
            on_mousedown : event => model.mousedown(event.clientX, event.clientY),
            on_mouseup   : event => model.mouseup  (event.clientX, event.clientY),
            on_mousemove : event => model.mousemove(event.clientX, event.clientY)
        }));
    };
    
    const begin_game_loop = module => {
        // main loop
        m_do_update = make_update_routine(module);
    };
    
    const prepare_html = (model, canvas_parent) => {
        const sel = (n) => canvas_parent.find(n);
        const [menu_html, on_click_scene_link] = prepare_scenes_menu(model);
        // this part is not so functional
        sel('#available-scenes').html(menu_html);
        sel('.scene-link').click(on_click_scene_link);
        
        let menu_tag = sel('#options-menu');
        sel('#open-menu').click(() => {
            menu_tag.addClass('slide-in').removeClass('slide-out');
        });
        sel('#close-menu').click(() => {
            menu_tag.removeClass('slide-in').addClass('slide-out');
        });
        sel('#intro-begin').click(me.play);
        
        m_events = make_event_methods(model, sel('.pause-overlay'));
        // all html prepared at this point...
        begin_game_loop(model);
        
        m_issue_resize();
    };
    
    // ----------------------- exposed functions/closures ----------------------
    
    me.setup = module => {
        const model = load_module_methods(module);
        const canvas = global_2d_canvas;
        $.ajax({ url: 'menus.html', type: "GET", dataType: 'text', 
            success: (data) => prepare_html(model, $('#everything-else').html(data).parent())
        });
        
        // initialize the program state on the C++ side
        model.start();
        
        // window resize event
        m_issue_resize = () => {
            $('#canvas').attr('height', canvas.height = $(window).innerHeight());
            $('#canvas').attr('width' , canvas.width  = $(window).innerWidth ());
            console.log("resize event w: " + canvas.width + " h: " + canvas.height);
            global_2d_context.font = 'bold 20pt Arial';
            model.canvas_resize(canvas.width, canvas.height);
            model.render_field();
            model.render_hud();            
        };
        $(window).resize(m_issue_resize);
        m_model = model;
    };
    
    me.issue_resize = () => m_issue_resize();
    
    me.play = () => {
        m_needs_to_pause = false;
        $('#pause-overlay').removeClass('overlay-fade-in').addClass('overlay-fade-out');
        $('.menu').removeClass('slide-in').addClass('slide-out');
        setTimeout(m_do_update, /* this has to match the css values... should use a getter somehow */ 1000);
    };
    
    me.pause = () => {
        $('#pause-overlay').addClass('overlay-fade-in').removeClass('overlay-fade-out');
        // $('.menu').addClass('slide-in').removeClass('slide-out');
        m_needs_to_pause = true;
    };
    
    // these should not be called before 'setup'?
    k_event_types.forEach(el =>
        me[el] = (event => m_events ? m_events[el](event) : undefined)
    );
    verify_dictionary_has_events(me);
    
    me.describe_components = comp_name => m_model.describe_components(comp_name);

    return Object.freeze(me);
}; // end of const make_program_state = ...

program_state = make_program_state();

})();
