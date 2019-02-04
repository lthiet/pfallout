open Utils
open Tsdl
open Texture_wrapper

module MMenu = struct
    let ev = Some (Sdl.Event.create ())

    type t = {
        bg_t : MTexture.t
    }

    let create_menu renderer path = 
        {
            bg_t = MTexture.load_from_file renderer path
        }

    type context = {
        over : bool
    }

    let update_context context = 
        if not (Sdl.poll_event ev) then
            match ev with
            (* If no event, nothing to do *)
            | None ->
                context
            (* Otherwise, check the event *)
            | Some e ->
                (* If the user clicks the red cross button, the game closes *)
                let over = check_ev_type e Sdl.Event.quit in
                {
                    over = over;
                }
        else
            context

    let rec loop menu renderer context = 
        if not context.over then
            (* Update the context *)
            let new_context = update_context context in
            (* Clear *)
            manage_result (Sdl.set_render_draw_color renderer 255 255 255 255) "Error : %s";
            manage_result (Sdl.render_clear renderer) "Error : %s";
        
            (* Display the menu background *)
            MTexture.render renderer menu.bg_t;

            (* Update the renderer *)
            Sdl.render_present renderer;

            (* Continue the game *)
            loop menu renderer new_context
    let menu_bg_path = "asset/image/menu_bg.png"
    let run renderer = 
        (* Create the menu *)
        let menu = create_menu renderer menu_bg_path in
        let menu_context  = {
            over = false
        } in
        loop menu renderer menu_context;
end;;