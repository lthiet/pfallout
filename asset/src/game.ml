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
open Fx
open Sdl_tools
open Entity_information
open Tsdl_ttf
open Interface


module MGame = struct

  (*  Render all the units in the list that is of layer l as a side effect,
      also returns the information about these units *)
  let render_units_and_return_info renderer textures scale camera frame faction_list layer =
    (* Iterate over each faction  *)
    List.fold_left (
      fun acc1 faction ->
        (* Iterate over each unit of a faction  *)
        let tmp = List.fold_left (
            fun acc2 entity ->
              if entity#check_layer layer then
                (* Render the unit *)
                let x,y,displayed = 
                  MEntity.render renderer entity textures scale (MCamera.get_rect camera) frame
                in
                let info = MEntity_information.get_info entity x y displayed in
                info :: acc2
              else
                acc2
          ) acc1 (MFaction.get_entity faction)
        in
        tmp @ acc1
    ) [] faction_list

  (* Render the animated and return the info of the animated *)
  let render_animated_and_return_info renderer textures scale camera frame animation = 
    List.fold_left (
      fun acc t ->  
        let pos_x,pos_y = MAnimation.next_coord_currently_animated t in
        (* Render the entity *)
        let entity = MAnimation.get_currently_animated t in
        let x,y,displayed = MEntity.render renderer 
            ~x:(Some pos_x)
            ~y:(Some pos_y)
            (MAnimation.get_currently_animated t) textures scale (MCamera.get_rect camera) frame
        in

        (* Render some effects *)
        let () = 
          let fx = MAnimation.get_current_fx t in
          match fx with
          | None -> ()
          | Some fx -> 
            MFx.render renderer fx textures scale (MCamera.get_rect camera) frame;
        in

        let info = MEntity_information.get_info entity x y displayed in
        (* Create the info and put it in the acc *)
        info :: acc
    ) [] (MAnimation.get_current_animated_and_next animation)


  (* Loop the game *)
  let rec loop renderer (context : MGameContext.t) textures = 
    if context.over then
      ()
    else
      let new_context = MGameContext.update_context context in
      let scaled_camera = MCamera.scale context.camera context.scale in

      (* Clear *)
      manage_result (Sdl.set_render_draw_color renderer 255 255 255 255) "Error : %s";
      manage_result (Sdl.render_clear renderer) "Error : %s";

      (* Render the background *)
      MBackground.render renderer (MTexture_pack.get_bg textures) context.scale (MCamera.get_rect scaled_camera);

      (* Render the tiles *)
      MGrid.render renderer textures context.grid context.scale (MCamera.get_rect scaled_camera) context.frame;


      (* Store all the information of each entity *)
      let info =
        (* Render the infrastructures *)
        let l1 = render_units_and_return_info renderer textures context.scale scaled_camera context.frame context.faction_list MLayer_enum.INFRASTRUCTURE in

        (* Render the military *)
        let l2 = render_units_and_return_info renderer textures context.scale scaled_camera context.frame context.faction_list MLayer_enum.MILITARY in

        (* Rendert he animated *)
        let l3 = render_animated_and_return_info renderer textures context.scale scaled_camera context.frame context.animation in
        l1 @ l2 @ l3
      in

      (* Render the info *)
      List.iter (fun x ->
          match x with
          | None -> ()
          | Some x ->
            MEntity_information.render renderer textures context.scale x
        ) info;

      (* Render the selector ( cursor ) *)
      MCursor.render renderer (MTexture_pack.get_curs textures) context.cursor_selector context.scale (MCamera.get_rect scaled_camera);

      (* Render the movement range selector *)
      List.iter (
        fun x -> let c = MCursor.create x#get_r x#get_q MCursor.POSSIBLE in
          MCursor.render renderer (MTexture_pack.get_curs textures) c context.scale (MCamera.get_rect scaled_camera);
      ) context.movement_range_selector;





      (*
      (* Display the user interface windows/buttons*)
      List.iter (
        fun t ->
          MInterface.render renderer t ,
      ) context.interface;
      *)

      (* Display the interface *)
      MInterface.render renderer context.interface textures;

      (* Update the renderer *)
      Sdl.render_present renderer;



      (* Continue the game *)
      loop renderer new_context textures


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
    let hp = MItem.create_healthpack rthp#get_r rthp#get_q 35 in
    MGrid.add_item_at grid hp;
    hp

  let create_random_nuke grid = 
    let rthp = MGrid.get_random_accessible_tile grid MLayer_enum.MILITARY () in
    let nuke = MItem.create_nuke rthp#get_r rthp#get_q 2 in
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
      let city1 = create_random_city grid faction_code2 in

      let faction2 =
        let f = MFaction.create_faction faction_code2 in
        MFaction.add_entity soldier3 f
        |> MFaction.add_entity soldier4
        |> MFaction.add_entity city1
      in

      let faction_code3 = 
        MFaction_enum.create MFaction_enum.USA
      in

      let soldier6 = create_random_soldier grid faction_code3 in
      let soldier7 = create_random_soldier grid faction_code3 in
      let city2 = create_random_city grid faction_code3 in

      let faction3 =
        let f = MFaction.create_faction faction_code3 in
        MFaction.add_entity soldier6 f
        |> MFaction.add_entity soldier7
        |> MFaction.add_entity city2
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
        scale = 1.;
        interface = MInterface.create_window 400 400 600 600
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
