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
    mousedown           : module.cwrap('js_glue_on_mouse_down'   , null    , ['number', 'number']),
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
    const pushl = (options_sel.find('input[name=push-level]').val() === 'true' ? '' : 'no-')
                 + 'show-push-level';
    // can be used directly
    return options_sel.find('input[name=iteration-algorithm]:checked').val()
           + ',' + ills + ',' + pushl;
};

const make_update_routines = model => {
    let interval;
    const start_loop = () => {
        console.log('game started');
        let old_time = new Date();
        interval = setInterval(() => {
            const new_time = new Date();
            model.time_update((new_time - old_time) / 1000);
            old_time = new_time;
            model.render_field();
            model.render_hud();
        }, 1000 / k_frame_rate);
    };
    const stop_loop = () => {
        clearInterval(interval);
        interval = undefined;
        if (interval !== undefined) throw 'did not go undefined';
    };
    return Object.freeze({
        start      : start_loop, 
        is_stopped : () => interval === undefined, 
        stop       : stop_loop
    });
};

const make_program_state = () => {
    let me = {};
    
    // let's try and aim for "as functional as possible"
    // since we can't be pure AND have interesting things happen...
    let m_do_update;
    let m_loop_methods;
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
    
    const make_event_methods = (model, pause_overlay) => {
        const make_act       = (n) => (f) => f(n); 
        const key_func_table = Object.freeze({
            'w' : make_act(0), 'a' : make_act(3), 's' : make_act(1),
            'd' : make_act(2), 'p' : make_act(4), 'o' : make_act(5)
        });
        
        return verify_dictionary_has_events(Object.freeze({
            on_keyup     : event => {
                //console.log('key up ' + event.key);
                (key_func_table[event.key] || (() => {}))(model.keyup  );
            },
            on_keydown   : event => {
                //console.log('key down ' + event.key);
                (key_func_table[event.key] || (() => {}))(model.keydown);
            },
            on_mousedown : event => model.mousedown(event.clientX, event.clientY),
            on_mouseup   : event => model.mouseup  (event.clientX, event.clientY),
            on_mousemove : event => model.mousemove(event.clientX, event.clientY)
        }));
    };
    
    const begin_game_loop = module => {
        // main loop
        m_loop_methods = make_update_routines(module);
    };
    
    const prepare_html = (model, canvas_parent) => {
        const sel = (n) => { 
            const rv = canvas_parent.find(n);
            if (rv.length < 1) throw 'Could not find any elements from ' + n;
            return rv;
        };
        const [menu_html, on_click_scene_link] = prepare_scenes_menu(model);
        // this part is not so functional
        sel('#available-scenes').html(menu_html);
        sel('.scene-link').click(on_click_scene_link);
        const close_menu = sel_str => {
            if ($(sel_str).length < 1) {
                throw 'could not find any elements from ' + sel_str;
            }
            const menu = $(sel_str).removeClass('slide-in').addClass('slide-out');
            menu.find('a').attr('tabindex', '-1');
            console.log('closing links ' + menu.find('a').length);
        };
        const open_menu = sel_str => {
            const menu = $(sel_str).addClass('slide-in').removeClass('slide-out');
            console.log('openning links ' + menu.find('a').length);
            menu.find('a').removeAttr('tabindex');
        };
        
        // all resume links
        sel('.resume-demo').click(function() {
            $(this).blur();
            me.play();
            close_menu('#options-menu');
            close_menu('#introduction');
            $('#pause-button a').removeAttr('tabindex');
            // ...but we have a "keyup" event that's gonna fire!
        });
        
        sel('#options-menu .close-menu').click(() => {
            close_menu('#options-menu');
        });
        sel('#options-menu .show-intro').click(function() {
            $(this).blur();
            // should be paused already
            open_menu ('#introduction');
            close_menu('#options-menu');
        });
        
        sel('#pause-overlay .show-options').click(() => {
            open_menu('#options-menu');
        });
        
        sel('#pause-button .pause-demo').click(function() {
            $(this).blur().find('a').attr('tabindex', '-1');
            me.pause();
            $('#pause-overlay').removeAttr('tabindex');
            $('#pause-button a').first().focus();
        });
        
        sel('#introduction .show-options').click(function() {
            $(this).blur();
            close_menu('#introduction');
            open_menu ('#options-menu');
        });
        m_events = make_event_methods(model, sel('#pause-overlay'));
        // all html prepared at this point...
        begin_game_loop(model);
        
        m_issue_resize();
    };
    
    // ----------------------- exposed functions/closures ----------------------
    
    me.setup = module => {
        const model = load_module_methods(module);
        const canvas = global_2d_canvas;
        $.ajax({ url: 'menus.html', type: "GET", dataType: 'text', 
            success: (data) => prepare_html(model, $('#menus').html(data).parent())
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
        $('#canvas').mouseup  (me.on_mouseup  );
        $('#canvas').mousedown(me.on_mousedown);
        $('#canvas').mousemove(me.on_mousemove);
        m_model = model;
    };
    
    me.issue_resize = () => m_issue_resize();
    
    me.play = () => {
        $('.menu').removeClass('slide-in').addClass('slide-out').attr('tabindex', '-1');
        $('#menus').removeClass('on-pause').addClass('on-play');
        setTimeout(m_loop_methods.start, 
            /* this has to match the css values... should use a getter somehow */ 1000);
    };
    
    me.pause = () => {
        $('#menus').addClass('on-pause').removeClass('on-play');
        m_loop_methods.stop();
        if (!m_loop_methods.is_stopped()) throw 'failed to pause';
    };
    
    // these should not be called before 'setup'?
    k_event_types.forEach(el =>
        me[el] = (event => {
            // if m_events is defined then so are m_loop_methods
            if (m_events === undefined) return;
            if (!m_loop_methods.is_stopped()) m_events[el](event);
        })
    );
    verify_dictionary_has_events(me);
    
    me.describe_components = comp_name => m_model.describe_components(comp_name);

    return Object.freeze(me);
}; // end of const make_program_state = ...

program_state = make_program_state();

})();
