"use strict";
// I may reduce this down to a single function call
let program_state = (() => {
    // instance dictionary
    let me = {};
    
    // something modest for js/wasm
    const k_frame_rate = 45;
        
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
        
        // this part is not so functional
        $('#available-scenes').html(menu_html);
        $('.scene-link').click(function () {
            load_scene(parseInt($(this).attr('scene-number')));
        });
    };
    
    // makes the function that starts
    const make_update_routine = module => {
        const do_time_update  = module.cwrap('js_glue_on_update'   , null, ['number']);
        const do_render_field = module.cwrap('js_glue_render_field', null, null);
        const do_render_hud   = module.cwrap('js_glue_render_hud'  , null, null);
        
        return () => {
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
    
    const make_event_methods = module => {
        const translate_key_code = (() => {
            const tbl = Object.freeze({
                'w': 1, 'a': 4, 's': 2, 'd': 3,
                'Enter': 5, 'q': 6
            });
            return key => (tbl[key] || 0) - 1;
        })();
        // I need to integrate menus...
        const do_keyup    = module.cwrap('js_glue_keyup'  , null, ['number']);
        const do_keydown  = module.cwrap('js_glue_keydown', null, ['number']);
        
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
            on_keyup    : event => do_keyup  (translate_key_code(event.key)),
            on_keydown  : event => do_keydown(translate_key_code(event.key))
        };
    };
    
    const setup_play_pause_link = () => {
        let ppbutton = $('#play-pause');
        const play = () => {
            program_state.play();
            ppbutton.text('pause');
            ppbutton.click(pause);
        };
        const pause = () => {
            program_state.pause();
            ppbutton.text('play');
            ppbutton.click(play);
        };
        ppbutton.click(pause);
    };
    
    // ----------------------- exposed functions/closures ----------------------
    
    me.setup = (module) => {
        const canvas = global_2d_canvas;
        
        // initialize the program state on the C++ side
        module.ccall('js_glue_start', null, null, null);
        m_do_inquiry = module.cwrap('js_describe_components', 'string', ['string']);
        prepare_scenes_menu(module);

        // window resize event
        const issue_canvas_resize = module.cwrap('js_glue_on_canvas_resize', null, ['number', 'number']);
        $(window).resize(m_issue_resize = () => {
            canvas.width  = $('.canvas-parent').width ();
            canvas.height = $('.canvas-parent').height();
            console.log("w: " + canvas.width + " h: " + canvas.height);
            global_2d_context.font = 'bold 20pt Arial';
            issue_canvas_resize(canvas.width, canvas.height);
        });
        m_issue_resize();

        m_events = make_event_methods(module);
        
        // main loop
        m_do_update = make_update_routine(module);
        issue_canvas_resize(canvas.width = 800, canvas.height = 600);
        global_2d_context.font = 'bold 20pt Arial';
        setup_play_pause_link();
        me.play();
        
        console.log('module started');
    };
    
    me.issue_resize = () => m_issue_resize();
    
    me.play = () => {
        m_needs_to_pause = false;
        m_do_update();
    };
    
    me.pause = () => m_needs_to_pause = true;
    
    m_events = {};
    ['on_keydown', 'on_keyup'].forEach(el => {
        me[el] = event => m_events[el](event);
    });
    
    me.describe_components = comp_name => m_do_inquiry(comp_name);

    return Object.freeze(me);
})();
