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
open Animation
open Entity
open Action_enum
open Texture_pack


module MGame = struct
  type textures = {
    tile : MTexture.t;
    terrain_feature : MTexture.t;
    bg : MTexture.t;
    curs : MTexture.t;
    soldier_eu : MTexture.t;
    soldier_pac : MTexture.t;
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
      MBackground.render renderer (MTexture_pack.get_bg textures) context.camera;

      (* Render the tiles *)
      MGrid.render renderer (MTexture_pack.get_tile textures) (MTexture_pack.get_terrain_feature textures) context.grid context.camera;

      (* Render the selector ( cursor ) *)
      MCursor.render renderer (MTexture_pack.get_curs textures) context.cursor_selector context.camera;

      (* Render the movement range selector *)
      List.iter (
        fun x -> let c = MCursor.create x#get_r x#get_q MCursor.POSSIBLE in
          MCursor.render renderer (MTexture_pack.get_curs textures) c context.camera
      ) context.movement_range_selector;



      (* Render the soldiers *)
      List.iter (
        fun x ->
          List.iter (
            fun y ->
              MEntity.render renderer y textures context.camera context.frame
          )
            (MFaction.get_entity x)
      ) context.faction_list;

      (* Render the animated *)
      List.iter (
        fun t ->  
          let pos_x,pos_y = MAnimation.next_coord_currently_animated t in
          MEntity.render renderer 
            ~x:(Some pos_x)
            ~y:(Some pos_y)
            (MAnimation.get_currently_animated t) textures context.camera context.frame
      ) (MAnimation.get_current_animated_and_next context.animation);

      (* Update the renderer *)
      Sdl.render_present renderer;

      (* Continue the game *)
      loop renderer new_context textures

  let tile_path = "asset/image/tiles.png"
  let terrain_feature_path = "asset/image/features.png"
  let bg_path = "asset/image/bg.png"
  let cursor_path = "asset/image/cursors.png"
  let soldier_eu_path = "asset/image/soldier-eu.png"
  let soldier_pac_path = "asset/image/soldier-pac.png"

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
        MFaction.add_entity soldier1 f
        |> MFaction.add_entity soldier2
      in

      let faction_code2 = 
        MFaction_enum.create MFaction_enum.ASIA
      in
      let soldier3 = MMilitary.create_soldier (start+2) (start+2) faction_code2 in
      let soldier4 = MMilitary.create_soldier (start+3) (start+2) faction_code2 in
      let faction2 =
        let f = MFaction.create_faction faction_code2 in
        MFaction.add_entity soldier3 f
        |> MFaction.add_entity soldier4
      in

      let faction_code3 = 
        MFaction_enum.create MFaction_enum.ASIA
      in
      let soldier5 = MMilitary.create_soldier (start+4) (start+4) faction_code3 in

      let faction3 =
        let f = MFaction.create_faction faction_code3 in
        MFaction.add_entity soldier5 f
      in

      let grid = MGrid.create start in

      let () =
        MGrid.add_mg_at grid soldier1;
        MGrid.add_mg_at grid soldier2;
        MGrid.add_mg_at grid soldier3;
        MGrid.add_mg_at grid soldier4;
        MGrid.add_mg_at grid soldier5;
      in

      let ctx : MGameContext.t = {
        over = false;
        camera = Sdl.Rect.create (start*MHex.size) (start*MHex.size) (screen_width) (screen_height);
        grid = grid;
        cursor_selector = MCursor.create start start MCursor.SELECTING;
        faction_list = [faction1;faction2;faction3];
        faction_controlled_by_player = faction1;
        action_src = None;
        action_dst = None;
        action_type = None;
        to_be_added = [];
        to_be_deleted = [];
        animation = MAnimation.create [];
        movement_range_selector = [];
        new_turn = false;
        frame = 0;
      } in

      let txt = 
        let tile = MTexture.load_from_file renderer tile_path in
        let terrain_feature = MTexture.load_from_file renderer terrain_feature_path in
        let bg = MTexture.load_from_file renderer bg_path in
        let curs = MTexture.load_from_file renderer cursor_path in
        let soldier_eu = MTexture.load_from_file renderer soldier_eu_path in
        let soldier_pac = MTexture.load_from_file renderer soldier_pac_path in
        MTexture_pack.create tile terrain_feature bg curs soldier_eu soldier_pac
      in
      loop renderer ctx txt
end
;;
