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
open Texture_pack
open Camera
open Item
open Layer_enum
open Sound


module MGame = struct
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
      MBackground.render renderer (MTexture_pack.get_bg textures) (MCamera.get_rect context.camera);

      (* Render the tiles *)
      MGrid.render renderer textures context.grid (MCamera.get_rect context.camera) context.frame;

      (* Render the selector ( cursor ) *)
      MCursor.render renderer (MTexture_pack.get_curs textures) context.cursor_selector (MCamera.get_rect context.camera);

      (* Render the movement range selector *)
      List.iter (
        fun x -> let c = MCursor.create x#get_r x#get_q MCursor.POSSIBLE in
          MCursor.render renderer (MTexture_pack.get_curs textures) c (MCamera.get_rect context.camera);
      ) context.movement_range_selector;

      (* TODO : factorize this if possible *)

      (* Render the infrastructures *)
      List.iter (
        fun x ->
          List.iter (
            fun y ->
              if y#check_layer MLayer_enum.INFRASTRUCTURE then
                MEntity.render renderer y textures (MCamera.get_rect context.camera) context.frame
          )
            (MFaction.get_entity x)
      ) context.faction_list;


      (* Render the military *)
      List.iter (
        fun x ->
          List.iter (
            fun y ->
              if y#check_layer MLayer_enum.MILITARY then
                MEntity.render renderer y textures (MCamera.get_rect context.camera) context.frame
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
            (MAnimation.get_currently_animated t) textures (MCamera.get_rect context.camera) context.frame
      ) (MAnimation.get_current_animated_and_next context.animation);

      (* Update the renderer *)
      Sdl.render_present renderer;

      (*
      (* Display the user interface windows/buttons*)
      List.iter (
        fun t ->
          MInterface.render renderer t ,
      ) context.interface;
      *)

      (* Continue the game *)
      loop renderer new_context textures

  let tile_path = "asset/image/tiles.png"
  let terrain_feature_path = "asset/image/features.png"
  let bg_path = "asset/image/bg.png"
  let cursor_path = "asset/image/cursors.png"
  let soldier_eu_path = "asset/image/soldier-eu.png"
  let soldier_pac_path = "asset/image/soldier-pac.png"
  let city_path = "asset/image/city.png"
  let healthpack_path = "asset/image/healthpack.png"

  (* Create a random soldier and adds it to the grid *)
  let create_random_soldier grid fc =
    let rts = MGrid.get_random_accessible_tile grid MLayer_enum.MILITARY () in
    let s = MMilitary.create_soldier (rts#get_r) (rts#get_q) fc in
    MGrid.add_at grid s;
    s

  (* Same as above except with city *)
  let create_random_city grid fc = 
    let rtc = MGrid.get_random_accessible_tile grid MLayer_enum.INFRASTRUCTURE () in
    let c = MInfrastructure.create_city (rtc#get_r) (rtc#get_q) fc in
    MGrid.add_at grid c;
    c

  let create_random_hp grid =
    let rthp = MGrid.get_random_accessible_tile grid MLayer_enum.MILITARY () in
    let hp = MItem.create_healthpack rthp#get_r rthp#get_q 40 in
    MGrid.add_item_at grid hp;
    hp

  let create_random_nuke grid = 
    let rthp = MGrid.get_random_accessible_tile grid MLayer_enum.MILITARY () in
    let nuke = MItem.create_nuke rthp#get_r rthp#get_q 1 in
    MGrid.add_item_at grid nuke;
    nuke





  (* Run the game with the correct paths and context *)
  let run (menu_result:MMenu.result) renderer = 
    if menu_result.start_game then
      let start = MMenu.get_map_size menu_result in
      let grid = MGrid.create start in

      (* Add some random items *)
      let _ = create_random_hp grid in
      let _ = create_random_hp grid in
      let _ = create_random_hp grid in
      let _ = create_random_nuke grid in
      let _ = create_random_nuke grid in

      let faction_code1 = 
        MFaction_enum.create MFaction_enum.EU
      in 
      let random_tile_soldier1 = MGrid.get_random_accessible_tile grid MLayer_enum.MILITARY ~bound:3 () in
      let soldier1 = MMilitary.create_soldier (random_tile_soldier1#get_r) (random_tile_soldier1#get_q) faction_code1 in
      MGrid.add_at grid soldier1;
      let random_tile_soldier2 = MGrid.get_random_accessible_tile grid MLayer_enum.MILITARY ~bound:3 () in
      let soldier2 = MMilitary.create_soldier (random_tile_soldier2#get_r) (random_tile_soldier2#get_q) faction_code1 in
      MGrid.add_at grid soldier2;

      let faction1 =
        let f = MFaction.create_faction faction_code1 in
        MFaction.add_entity soldier1 f 
        |> MFaction.add_entity soldier2
      in

      let faction_code2 = 
        MFaction_enum.create MFaction_enum.ASIA
      in

      let soldier3 = create_random_soldier grid faction_code2 in
      let soldier4 = create_random_soldier grid faction_code2 in
      (* let city1 = create_random_city grid faction_code2 in *)

      let faction2 =
        let f = MFaction.create_faction faction_code2 in
        MFaction.add_entity soldier3 f
        |> MFaction.add_entity soldier4
        (* |> MFaction.add_entity city1 *)
      in

      let faction_code3 = 
        MFaction_enum.create MFaction_enum.EU
      in

      let soldier6 = create_random_soldier grid faction_code3 in
      let soldier7 = create_random_soldier grid faction_code3 in
      (* let city2 = create_random_city grid faction_code3 in *)

      let faction3 =
        let f = MFaction.create_faction faction_code3 in
        MFaction.add_entity soldier6 f
        |> MFaction.add_entity soldier7
        (* |> MFaction.add_entity city2 *)
      in

      let camera_rect =
        let sw,sh = 
          Sdl.get_window_size (MMenu.get_window menu_result)
        in
        Sdl.Rect.create (start*MHex.size) (start*MHex.size) sw sh in

      let ctx : MGameContext.t = {
        over = false;
        camera = MCamera.create camera_rect;
        grid = grid;
        cursor_selector = MCursor.create start start MCursor.SELECTING;
        faction_list = [faction1;faction2;faction3];
        faction_controlled_by_player = faction1;
        action_src = None;
        action_dst = None;
        action_type = None;
        action_layer = None;
        to_be_added = [];
        to_be_deleted = [];
        animation = MAnimation.create [];
        movement_range_selector = [];
        new_turn = false;
        frame = 0;
        current_layer = MLayer_enum.MILITARY;
        window = MMenu.get_window menu_result;
      } in

      let txt = MTexture_pack.create renderer in


      (* Section to play music, WIP *)
      let () =
        let sound_pack = MSound.create_pack () in
        let music = MSound.get_music sound_pack in
        MSound.play music
      in
      loop renderer ctx txt
end
;;
