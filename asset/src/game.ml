open Texture_wrapper
open Keyboard_wrapper
open Tsdl
open Grid
open Background
open Cursor
open Utils
open Menu
open Hex
open Pathfinder
open Faction
open Faction_enum
open Action
open Military
open Infrastructure


module MGame = struct
    let ev = Some (Sdl.Event.create ())

    type context = {
        over : bool;
        camera : Sdl.rect;
        grid : MGrid.t;
        cursor_selector : MCursor.cursor;
        cursor_selector_dst : MCursor.cursor;
        player_turn : bool;
        range : int;
        faction_list : MFaction.t list;
    }

    type textures = {
        tile : MTexture.t;
        terrain_feature : MTexture.t;
        bg : MTexture.t;
        curs : MTexture.t
    }

    (* Return a new camera based on user input *)
    let get_camera e c =
        (* The distance at which the camera will move *)
        let offset = 10 in
        (* Check if event is a keydown *)
        if check_ev_type e Sdl.Event.key_down then
            (* If yes, check which key has been pressed *)
            let pressed_key = MKeyboard.get_scancode e in
            let x = new_int pressed_key Sdl.Scancode.d Sdl.Scancode.a  offset (Sdl.Rect.x c) in
            let y = new_int pressed_key Sdl.Scancode.s Sdl.Scancode.w  offset (Sdl.Rect.y c) in
            Sdl.Rect.create x y (Sdl.Rect.w c) (Sdl.Rect.h c)
        else
            c

    type keyset = {
        up : Sdl.scancode;
        down : Sdl.scancode;
        left : Sdl.scancode;
        right : Sdl.scancode;
    }

    (*
        e : event
        c : cursor
        ks : key_set
        g : grid
    *)
    let move_cursor_io e c ks (g:MGrid.t) =
        let offset = 1 in
        if check_ev_type e Sdl.Event.key_down then
            let pk = MKeyboard.get_scancode e in
            let r = new_int pk ks.down ks.up  offset c#get_r in
            let q = new_int pk ks.right ks.left  offset c#get_q in
            let tile_below = MGrid.get_tile r q g in
            if tile_below#is_impassable then
                c
            else
                MCursor.move c r q
        else
            c

    let get_player_turn e pt =
        if check_ev_type e Sdl.Event.key_down then
            let pk = MKeyboard.get_scancode e in
            if pk = Sdl.Scancode.return then
                not pt
            else
                pt
        else
            pt

    let get_range e r =
        if check_ev_type e Sdl.Event.key_down then
            let pk = MKeyboard.get_scancode e in
            max (new_int pk Sdl.Scancode.o Sdl.Scancode.l 1 r) 0
        else
            r

        
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
                let camera = get_camera e context.camera in
                let cursor_selector_ks = {
                    up = Sdl.Scancode.up;
                    down = Sdl.Scancode.down;
                    right = Sdl.Scancode.right;
                    left = Sdl.Scancode.left;
                } in
                let cursor_selector_dst_ks = {
                    up = Sdl.Scancode.y;
                    down = Sdl.Scancode.h;
                    right = Sdl.Scancode.j;
                    left = Sdl.Scancode.g;
                } in

                let cursor_selector = move_cursor_io e context.cursor_selector cursor_selector_ks context.grid in
                let cursor_selector_dst = move_cursor_io e context.cursor_selector_dst cursor_selector_dst_ks context.grid in
                let player_turn = get_player_turn e context.player_turn in
                let range = get_range e context.range in
                {
                    context with
                    over = over;
                    camera = camera;
                    cursor_selector = cursor_selector;
                    cursor_selector_dst = cursor_selector_dst;
                    player_turn = player_turn;
                    range = range;
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
            MGrid.render renderer textures.tile textures.terrain_feature context.grid context.camera;

            (* Render the selector ( cursor ) *)
            (
                MCursor.render renderer textures.curs context.cursor_selector_dst context.camera context.grid;
                MCursor.render renderer textures.curs context.cursor_selector context.camera context.grid;

                if context.player_turn then
                (
                                       (* let ranged_cursor_coords = MHex.range_ax context.range context.cursor_selector#get_axial in
                    List.iter ( fun e ->
                        let e = MHex.cube_to_axial e in
                        let ranged_cursor = MCursor.create e.r e.q MCursor.POSSIBLE in
                        MCursor.render renderer textures.curs ranged_cursor context.camera context.grid
                    ) ranged_cursor_coords; *)

                    (* let line = MHex.cube_linedraw (context.cursor_selector#get_cube) (context.cursor_selector_dst#get_cube) in
                    List.iter ( fun e ->
                        let e = MHex.cube_to_axial e in
                        let x = MCursor.create e.r e.q MCursor.IMPOSSIBLE in
                        MCursor.render renderer textures.curs x context.camera context.grid
                    ) line; *)

                    let tile_below_src = MGrid.get_tile context.cursor_selector#get_r context.cursor_selector#get_q context.grid in
                    let tile_below_dst = MGrid.get_tile context.cursor_selector_dst#get_r context.cursor_selector_dst#get_q context.grid in
                    if not (tile_below_src#is_impassable || tile_below_dst#is_impassable)then
                        begin
                        for i = 0 to context.range do
                        let reachable_tiles = MPathfinder.reachable_tile (tile_below_src) (context.grid) i in
                        List.iter (
                            fun x ->
                                let x = MCursor.create x#get_r x#get_q MCursor.POSSIBLE in
                                MCursor.render renderer textures.curs x context.camera context.grid
                        ) reachable_tiles
                        done;

                        let path = MPathfinder.a_star tile_below_src tile_below_dst context.grid in
                        List.iter (
                            fun x ->
                                let x = MCursor.create x#get_r x#get_q MCursor.IMPOSSIBLE in
                                MCursor.render renderer textures.curs x context.camera context.grid
                        ) path;
                        ()
                        end
                )
             );



            (* Update the renderer *)
            Sdl.render_present renderer;

            (* Continue the game *)
            loop renderer new_context textures

    let tile_path = "asset/image/tiles.png"
    let terrain_feature_path = "asset/image/features.png"
    let bg_path = "asset/image/bg.png"
    let cursor_path = "asset/image/cursors.png"

    (* Run the game with the correct paths and context *)
    let run (menu_result:MMenu.result) renderer screen_width screen_height = 
        if menu_result.start_game then
            let start = 7 in

            let faction1 =
                let f = MFaction.create_faction MFaction_enum.EU true in
                let m = MMilitary.create_soldier start start in
                MFaction.add_military f m
            in

            let () =
                let tmp = MFaction.get_military faction1 in
                match tmp with
                | [] -> Printf.printf "%s" "ok";
                | x :: s -> Printf.printf "%d" x#get_pc
            in

            let ctx = {
                over = false;
                camera = Sdl.Rect.create (start*MHex.size) (start*MHex.size) (screen_width) (screen_height);
                grid = MGrid.create start;
                cursor_selector = MCursor.create start start MCursor.SELECTING;
                cursor_selector_dst = MCursor.create (start+2) (start+2) MCursor.SELECTING;
                player_turn = true;
                range = 1;
                faction_list = [faction1]
            } in

            let txt = {
                tile = MTexture.load_from_file renderer tile_path;
                terrain_feature = MTexture.load_from_file renderer terrain_feature_path;
                bg = MTexture.load_from_file renderer bg_path;
                curs = MTexture.load_from_file renderer cursor_path
            } in
            loop renderer ctx txt
end
;;
