open Texture_wrapper
open Keyboard_wrapper
open Tsdl
open Grid
open Background
open Cursor
open Utils
open Menu


module MGame = struct
    let ev = Some (Sdl.Event.create ())

    type context = {
        over : bool;
        camera : Sdl.rect;
        grid : MGrid.t;
        cursor_selector : MCursor.cursor
    }

    type textures = {
        tile : MTexture.t;
        bg : MTexture.t;
        curs : MTexture.t
    }

    (* Update the new context of the game *)
    let update_context context =
        (* Get the next event in the queue *)
        if  (Sdl.poll_event ev) then
            match ev with
            (* If no event, nothing to do *)
            | None ->
                context
            (* Otherwise, check the event *)
            | Some e ->
                (* If the user clicks the red cross button, the game closes *)
                let over = check_ev_type e Sdl.Event.quit in
                let camera = MKeyboard.get_camera e context.camera in
                {
                    context with
                    over = over;
                    camera = camera
                }
        else
            context


    (* Loop the game *)
    let rec loop renderer context textures = 
        if context.over then
            ()
        else
            let new_context = update_context context in

            (* Clear *)
            manage_result (Sdl.set_render_draw_color renderer 255 255 255 255) "Error : %s";
            manage_result (Sdl.render_clear renderer) "Error : %s";

            (* Render the background *)
            MBackground.render renderer textures.bg context.camera;

            (* Render the tiles *)
            MGrid.render renderer textures.tile context.grid context.camera;

            (* Render the selector ( cursor ) *)
            MCursor.render renderer textures.curs context.cursor_selector context.camera;

            (* Update the renderer *)
            Sdl.render_present renderer;

            (* Continue the game *)
            loop renderer new_context textures

    let tile_path = "asset/image/tiles.png"
    let bg_path = "asset/image/bg.png"
    let cursor_path = "asset/image/cursors.png"

    (* Run the game with the correct paths and context *)
    let run (menu_result:MMenu.result) renderer screen_width screen_height = 
        if menu_result.start_game then
            let ctx = {
                over = false;
                camera = Sdl.Rect.create 0 0 (screen_width) (screen_height);
                grid = MGrid.create 5;
                cursor_selector = MCursor.create 4 4
            } in

            let txt = {
                tile = MTexture.load_from_file renderer tile_path;
                bg = MTexture.load_from_file renderer bg_path;
                curs = MTexture.load_from_file renderer cursor_path
            } in
            loop renderer ctx txt
end
;;
