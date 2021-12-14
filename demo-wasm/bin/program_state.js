"use strict";
// I may reduce this down to a single function call
let program_state = (() => {
    // instance dictionary
    let me = {};
    
    // something modest for js/wasm
    const k_frame_rate = 40;
        
    let m_do_render_field;
    let m_do_render_hud;
    let m_do_update;
    let m_do_keyup;
    let m_do_keydown;
    let m_needs_to_pause = false;

    // ----------------------- hidden functions/closures -----------------------
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
    
    let update = () => {
        if (m_needs_to_pause) return;
        m_do_update(1 / k_frame_rate);
        m_do_render_field();
        m_do_render_hud();
        setTimeout(update, 1000 / k_frame_rate);
    };
    
    // ----------------------- exposed functions/closures ----------------------
    
    me.setup = (module) => {
        let canvas = global_2d_canvas;
        
        // initialize the program state on the C++ side
        module.ccall('js_glue_start', null, null, null);
        
        // init_mouse_events(module, canvas);
        
        // window resize event
        let issue_canvas_resize = module.cwrap('js_glue_on_canvas_resize', null, ['number', 'number']);
        // should be set in the html
        canvas.width  = 800;
        canvas.height = 400;
        canvas.addEventListener('resize', (_ignored_event) => {
            console.log("w: " + canvas.width + " h: " + canvas.height);
            issue_canvas_resize(canvas.width, canvas.height);
            global_2d_context.font = 'bold 20pt Arial';
        });
        issue_canvas_resize(canvas.width, canvas.height);

        //m_change_scene = module.cwrap('js_glue_change_scene', null, ['string']);
        //m_change_behavior = module.cwrap('js_glue_change_behavior', null, ['string']);
        m_do_keyup    = module.cwrap('js_glue_keyup'  , null, ['number']);
        m_do_keydown  = module.cwrap('js_glue_keydown', null, ['number']);
        
        // main loop
        m_do_update       = module.cwrap('js_glue_on_update'   , null, ['number']);
        m_do_render_field = module.cwrap('js_glue_render_field', null, null);
        m_do_render_hud   = module.cwrap('js_glue_render_hud'  , null, null);
        global_2d_context.font = 'bold 20pt Arial';
        me.play();
        
        console.log('module started');
    };
    
    me.play = () => {
        m_needs_to_pause = false;
        update();
    };
    
    me.pause = () => m_needs_to_pause = true;
    
    const translate_key_code = (() => {
        const tbl = Object.freeze({
            'w': 0,
            'a': 3,
            's': 1,
            'd': 2
        });
        return key => tbl[key];
    })();
    
    
    me.on_keydown = (event) => m_do_keydown(translate_key_code(event.key));
    
    me.on_keyup = (event) => m_do_keyup(translate_key_code(event.key));

    return me;
})();
