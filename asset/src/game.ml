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
open Animation
open Entity
open Action_enum


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
      MCursor.render renderer textures.curs context.cursor_selector context.camera;

      (* Render the movement range selector *)
      List.iter (
        fun x -> let c = MCursor.create x#get_r x#get_q MCursor.POSSIBLE in
          MCursor.render renderer textures.curs c context.camera
      ) context.movement_range_selector;



      (* Render the soldiers *)
      List.iter (
        fun x ->
          List.iter (
            fun y ->
              MMilitary.render renderer y textures.military context.camera 0
          )
            (MFaction.get_military x)
      ) 
        context.faction_list;

      (* Render the animated *)
      List.iter (
        fun x ->  MEntity.render renderer x textures.military context.camera 0
      ) (MAnimation.get_current_animated context.animation);

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
      let start = 8 in
      let faction_code1 = 
        MFaction_enum.create MFaction_enum.EU
      in 
      let soldier1 = MMilitary.create_soldier start start faction_code1 in
      let soldier2 = MMilitary.create_soldier (start+1) start faction_code1 in
      let faction1 =
        let f = MFaction.create_faction faction_code1 in
        MFaction.add_military soldier1 f
        |> MFaction.add_military soldier2
      in

      let faction_code2 = 
        MFaction_enum.create MFaction_enum.EU
      in
      let soldier3 = MMilitary.create_soldier (start+2) (start+2) faction_code2 in
      let soldier4 = MMilitary.create_soldier (start+3) (start+2) faction_code2 in

      let faction2 =
        let f = MFaction.create_faction faction_code2 in
        MFaction.add_military soldier3 f
        |> MFaction.add_military soldier4
      in

      let grid = MGrid.create start in

      let () =
        MGrid.add_mg_at grid soldier1;
        MGrid.add_mg_at grid soldier2;
        MGrid.add_mg_at grid soldier3;
        MGrid.add_mg_at grid soldier4;
      in

      let ctx : MGameContext.t = {
        over = false;
        camera = Sdl.Rect.create (start*MHex.size) (start*MHex.size) (screen_width) (screen_height);
        grid = grid;
        cursor_selector = MCursor.create start start MCursor.SELECTING;
        player_turn = true;
        new_turn = false;
        faction_list = [faction1;faction2];
        faction_controlled_by_player = faction1;
        action_src = None;
        action_dst = None;
        action_type = None;
        to_be_added_m = [];
        animation = MAnimation.create [];
        movement_range_selector = [];
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
