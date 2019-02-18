open Utils
open Tsdl
(* open Tsdl_ttf *)
open Texture_wrapper
open Keyboard_wrapper
open Mouse_wrapper
open Btn

module MMenu = struct
    let ev = Some (Sdl.Event.create ())

    type context = {
        over : bool;
        btn_start : MBtn.t 
    }

    type textures = {
        bg : MTexture.t;
        btn : MTexture.t;
        (* btn_start_text : MTexture.t *)
    }

    let update_context context = 
        if Sdl.poll_event ev then
            match ev with
            (* If no event, nothing to do *)
            | None ->
                context
            (* Otherwise, check the event *)
            | Some e ->

                (* Check if user mouse is above start button *)
                let x,y,w,h = MBtn.get_coord context.btn_start in
                let btn_start =
                    if MMouse.is_inside e x y w h then 
                        if check_ev_type e Sdl.Event.mouse_button_down then
                        {
                            context.btn_start with
                            status = MBtn.PRESSED
                        }
                        else if check_ev_type e Sdl.Event.mouse_button_up then
                        {
                            context.btn_start with
                            status = MBtn.RELEASED
                        }
                        else
                        context.btn_start
                    else
                        {
                            context.btn_start with
                            status = MBtn.IDLE
                        }
                in
                (* If the user clicks the red cross button, the game closes *)
                let over = check_ev_type e Sdl.Event.quit || MBtn.is_released btn_start in
                {
                    over = over;
                    btn_start = btn_start
                }
        else
            context

    type result = {
        start_game : bool
    }

    let compute_result ctx =
        {
            start_game = MBtn.is_released ctx.btn_start
        }


    let rec loop renderer context textures = 
        let new_ctx = update_context context in
        if not new_ctx.over then (
            (* Update the context *)
            (* Clear *)
            manage_result (Sdl.set_render_draw_color renderer 255 255 255 255) "Error : %s";
            manage_result (Sdl.render_clear renderer) "Error : %s";
        
            (* Display the menu background *)
            MTexture.render renderer textures.bg;

            (* Display the start button *)
            MBtn.render renderer new_ctx.btn_start textures.btn;
            (* Display the start button text *)
            (* MBtn.render_text renderer new_ctx.btn_start textures.btn_start_text; *)

            (* Update the renderer *)
            Sdl.render_present renderer;

            (* Continue the game *)
            loop renderer new_ctx textures
        )
        else
            compute_result new_ctx

    let menu_bg_path = "asset/image/menu_bg.png"
    let btn_path = "asset/image/btns.png"
    (* let font_path = "asset/font/spiderman.ttf" *)

    let run renderer = 
        (* let font = manage_result (Ttf.open_font font_path 70) "Error font %s" in *)
        (* Create the menu *)
        let txt = {
            bg = MTexture.load_from_file renderer menu_bg_path;
            btn = MTexture.load_from_file renderer btn_path;
            (* btn_start_text = MTexture.load_from_rendered_text renderer font "Start" (Sdl.Color.create 255 255 255 255) *)
        } in
        let ctx  = {
            over = false;
            btn_start = MBtn.create (960-(MBtn.width/2)) 1000 MBtn.START
        } in
        loop renderer ctx txt;
end;;