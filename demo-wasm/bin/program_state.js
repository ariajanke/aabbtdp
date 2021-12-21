"use strict";
// I may reduce this down to a single function call
let program_state = (() => {
    // instance dictionary
    let me = {};
    
    // something modest for js/wasm
    const k_frame_rate = 45;
        
    // let's try and aim for "as functional as possible"
    // since we can't be pure AND have interesting things happen...
    let m_do_update;
    let m_needs_to_pause = false;
    let m_events;
    let m_do_inquiry;
    let m_issue_resize;

    // ----------------------- hidden functions/closures -----------------------

    const prepare_scenes_menu = module => {
        // lets try to do more functional style of programming here c:
        const load_scene_names = module => {
            const get_scene_name = module.cwrap('js_glue_get_scene_name', 'string', ['number']);
            const get_at_index = (n, arr) => {
                const name = get_scene_name(n);
                if (name === '') return arr;
                
                return [name].concat( get_at_index(n + 1, arr) );
            };
            return get_at_index(0, []);
        };
        const load_scene = module.cwrap('js_glue_load_scene', null, ['number']);
        const menu_html = load_scene_names(module)
            .map((name, idx) => '<a class="scene-link" href="#" scene-number="' + idx + '">' + name + '</a><br>')
            .reduce((bulk, el) => bulk + el);
        
        return [menu_html, function () {
            load_scene(parseInt($(this).attr('scene-number')));
            $(this).blur();
            $('#options-menu').removeClass('slide-in').addClass('slide-out');
            me.play();
        }];

    };
    
    // makes the function that starts
    const make_update_routine = module => {
        const do_time_update  = module.cwrap('js_glue_on_update'   , null, ['number']);
        const do_render_field = module.cwrap('js_glue_render_field', null, null);
        const do_render_hud   = module.cwrap('js_glue_render_hud'  , null, null);
        
        return () => {
            console.log('game started');
            let old_time = new Date();
            const interval = setInterval(() => {
                if (m_needs_to_pause) {
                    clearInterval(interval);
                    return;
                }
                const new_time = new Date();
                do_time_update((new_time - old_time) / 1000);
                old_time = new_time;
                do_render_field();
                do_render_hud();
            }, 1000 / k_frame_rate);
        };
    };
    
    const make_event_methods = (module, pause_overlay) => {
        // I need to integrate menus...
        const do_keyup    = module.cwrap('js_glue_keyup'  , null, ['number']);
        const do_keydown  = module.cwrap('js_glue_keydown', null, ['number']);
        const make_act    = (n) => (f) => f(n); 
        const key_func_table = Object.freeze({
            'w' : make_act(0), 'a' : make_act(3), 's' : make_act(1),
            'd' : make_act(2), 'p' : make_act(4), 'o' : make_act(5),
            // pausing is pretty special
            'Enter' : state_act => {
                if (state_act !== do_keyup) return;
                (pause_overlay.hasClass('overlay-fade-in') ? me.play : me.pause)();
            }
        });
        
        if (false) {
            let init_mouse_events = (module, canvas) => {
                let issue_click = module.cwrap('js_glue_on_mouse_down', null, ['number', 'number']);
                canvas.onmousedown = e => issue_click(e.clientX, e.clientY);
        
                let issue_up = module.cwrap('js_glue_on_mouse_up', null, ['number', 'number']);
                canvas.onmouseup = e => issue_up(e.clientX, e.clientY);
        
                let issue_move = module.cwrap('js_glue_on_mouse_move', null, ['number', 'number']);
                canvas.onmousemove = e => issue_move(e.clientX, e.clientY);
            };
        }
        
        return {
            on_keyup   : event => (key_func_table[event.key] || (() => {}))(do_keyup  ),
            on_keydown : event => (key_func_table[event.key] || (() => {}))(do_keydown)
        };
    };
    
    const begin_game_loop = module => {
        // main loop
        m_do_update = make_update_routine(module);
        
    };
    
    const map_events = canvas_parent => {
        
    };
    
    const change_class_for = (element, old_class, new_class) => {
        if (element.hasClass(old_class)) {
            element.removeClass(old_class);
            element.addClass(new_class);
        }
    };
    
    const prepare_html = (module, canvas_parent) => {
        const sel = (n) => canvas_parent.find(n);
        const [menu_html, on_click_scene_link] = prepare_scenes_menu(module);
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
        
        m_events = make_event_methods(module, sel('.pause-overlay'));
        // all html prepared at this point...
        begin_game_loop(module);
        
        m_issue_resize();
    };
    
    // ----------------------- exposed functions/closures ----------------------
    
    me.setup = (module) => {
        const canvas = global_2d_canvas;
        $.ajax({ url: 'menus.html', type: "GET", dataType: 'text', 
            success: (data) => prepare_html(module, $('#everything-else').html(data).parent())
        });
        
        // initialize the program state on the C++ side
        module.ccall('js_glue_start', null, null, null);
        m_do_inquiry = module.cwrap('js_describe_components', 'string', ['string']);
        
        // window resize event
        const issue_canvas_resize = module.cwrap('js_glue_on_canvas_resize', null, ['number', 'number']);
        $(window).resize(m_issue_resize = () => {
            $('#canvas').attr('height', canvas.height = $(window).innerHeight());
            $('#canvas').attr('width' , canvas.width  = $(window).innerWidth ());
            console.log("resize event w: " + canvas.width + " h: " + canvas.height);
            global_2d_context.font = 'bold 20pt Arial';
            issue_canvas_resize(canvas.width, canvas.height);
        });
        
        //issue_canvas_resize(canvas.width = 800, canvas.height = 600);
        //global_2d_context.font = 'bold 20pt Arial';
        
        
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
    
    m_events = {};
    ['on_keydown', 'on_keyup'].forEach(el => {
        me[el] = event => m_events ? m_events[el](event) : undefined;
    });
    
    me.describe_components = comp_name => m_do_inquiry(comp_name);

    return Object.freeze(me);
})();
