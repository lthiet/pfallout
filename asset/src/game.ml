open Texture_wrapper
open Keyboard_wrapper
open Context
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
open Military
open Infrastructure


module MGame = struct

    type textures = {
        tile : MTexture.t;
        terrain_feature : MTexture.t;
        bg : MTexture.t;
        curs : MTexture.t;
        military : MTexture.t
    }

    (* Loop the game *)
    let rec loop renderer (context : MGameContext.t) textures = 
        if context.over then
            ()
        else
            let new_context = MGameContext.update_context context in

            (* Clear *)
            manage_result (Sdl.set_render_draw_color renderer 255 255 255 255) "Error : %s";
            manage_result (Sdl.render_clear renderer) "Error : %s";

            (* Render the background *)
            MBackground.render renderer textures.bg context.camera;

            (* Render the tiles *)
            MGrid.render renderer textures.tile textures.terrain_feature context.grid context.camera;

            (* Render the selector ( cursor ) *)
            MCursor.render renderer textures.curs context.cursor_selector context.camera context.grid;

            List.iter (
                fun x ->
                    List.iter (
                        fun y ->
                            MMilitary.render renderer y textures.military context.camera
                    )
                    (MFaction.get_military x)
            ) 
            context.faction_list;

            (* Update the renderer *)
            Sdl.render_present renderer;

            (* Continue the game *)
            loop renderer new_context textures

    let tile_path = "asset/image/tiles.png"
    let terrain_feature_path = "asset/image/features.png"
    let bg_path = "asset/image/bg.png"
    let cursor_path = "asset/image/cursors.png"
    let military_path = "asset/image/soldier.png"

    (* Run the game with the correct paths and context *)
    let run (menu_result:MMenu.result) renderer screen_width screen_height = 
        if menu_result.start_game then
            let start = 7 in

            let soldier1 = MMilitary.create_soldier start start in
            let faction1 =
                let f = MFaction.create_faction MFaction_enum.EU true in
                MFaction.add_military f soldier1
            in

            let grid = MGrid.create start in

            let () =
                MGrid.set_mg_at grid soldier1#get_r soldier1#get_q soldier1
            in

            let ctx : MGameContext.t = {
                over = false;
                camera = Sdl.Rect.create (start*MHex.size) (start*MHex.size) (screen_width) (screen_height);
                grid = grid;
                cursor_selector = MCursor.create start start MCursor.SELECTING;
                player_turn = true;
                faction_list = [faction1];
                action_src = None;
                action_dst = None
            } in

            let txt = {
                tile = MTexture.load_from_file renderer tile_path;
                terrain_feature = MTexture.load_from_file renderer terrain_feature_path;
                bg = MTexture.load_from_file renderer bg_path;
                curs = MTexture.load_from_file renderer cursor_path;
                military = MTexture.load_from_file renderer military_path;
            } in
            loop renderer ctx txt
end
;;
