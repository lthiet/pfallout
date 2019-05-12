open Keyboard_wrapper
open Utils
open Grid
open Cursor
open Faction
open Tsdl
open Hex
open Tile
open Action
open Action_enum
open Animation
open Military
open Infrastructure
open Entity
open Entity_enum
open Pathfinder
open Behaviour
open Camera
open Layer_enum
open Item
open Inventory
open Interface
open Tree

let ev = Some (Sdl.Event.create ())

(* The context module is the main engine of the game. Depending
   on multiples factors, such as input, it modifies the state of the game *)
module MGameContext = struct
  type t = {
    over : bool;
    camera : MCamera.t;
    grid : MGrid.t;
    cursor_selector : MCursor.cursor;
    faction_list : MFaction.t list;
    faction_controlled_by_player : MFaction.t;
    action_src : MHex.axial_coord option;
    action_dst : MHex.axial_coord option;
    action_layer : MLayer_enum.t option;
    action_type : MAction_enum.enum option;
    movement_range_selector : MTile.t list;
    to_be_added : MEntity.t list;
    to_be_deleted : MEntity.t list;
    animation : MAnimation.t;
    new_turn : bool;
    frame : int;
    scale : float;
    interface : MInterface.structure;
    current_layer : MLayer_enum.t;
    window : Sdl.window;
  }

  exception Nothing

  let action_src_is_set ctx =
    match ctx.action_src with
    | None -> false
    | _ -> true

  let action_dst_is_set ctx =
    match ctx.action_dst with
    | None -> false
    | _ -> true 

  let action_type_is_set ctx =
    match ctx.action_type with
    | None -> false
    | _ -> true 


  (* Return a new camera based on user input *)
  let get_camera ev window camera scale =
    MCamera.change_direction camera ev |> MCamera.update_camera window

  type keyset = {
    up : Sdl.scancode;
    down : Sdl.scancode;
    left : Sdl.scancode;
    right : Sdl.scancode;
  }

  (* Checks whether or not it is the player turn. it is decided
     by whether or turn the faction controlled by the player is that the
     head of the faction list *)
  let is_player_turn ctx =
    match ctx.faction_list with
    | [] -> false
    | x :: s ->
      MFaction.equal ctx.faction_controlled_by_player x
    (*
        e : event
        c : cursor
        ks : key_set
        g : grid
    *)
  let update_cs e ctx ks =
    let offset = 1 in
    let tmp = ctx.cursor_selector in
    let g = ctx.grid in
    let c =
      if action_src_is_set ctx then
        tmp#set_status MCursor.SELECTING_DST
      else
        tmp#set_status MCursor.SELECTING
    in

    if MAnimation.is_over ctx.animation  && (is_player_turn ctx) then
      begin
        if check_ev_type e Sdl.Event.key_down then
          let pk = MKeyboard.get_scancode e in
          let r = new_int pk ks.down ks.up  offset c#get_r in
          let q = new_int pk ks.right ks.left  offset c#get_q in
          let new_tile_below = MGrid.get_tile r q g in
          (* Checks whether or not the new tile is
             inside the tiles that represents the range of
             a certain action *)
          let new_tile_in_range = 
            List.exists (fun x ->
                x#get_axial = new_tile_below#get_axial
              ) ctx.movement_range_selector
          in
          if ((action_src_is_set ctx) && not new_tile_in_range) then
            c
          else				
            c#move r q
        else
          c
      end
    else
      c#set_status MCursor.HIDDEN

  (* Cancelled action is triggered when a user presses
     presses the escape key *)
  let action_cancelled e =
    MKeyboard.key_is_pressed e Sdl.Scancode.escape

  exception Entity_Not_Owned_By_Player

  let set_action_src e ctx =
    if (not (action_src_is_set ctx)) && MKeyboard.key_is_pressed e Sdl.Scancode.return && MAnimation.is_over ctx.animation then
      begin
        let ent_below = MGrid.get_at_ax ctx.grid ctx.cursor_selector#get_axial ctx.current_layer in
        if (MFaction.entity_in ent_below ctx.faction_controlled_by_player) then
          Some ctx.cursor_selector#get_axial
        else
          raise Entity_Not_Owned_By_Player
      end
    else if action_cancelled e then
      None
    else
      ctx.action_src

  let set_action_layer e ctx = 
    if (not (action_src_is_set ctx)) && MKeyboard.key_is_pressed e Sdl.Scancode.return && MAnimation.is_over ctx.animation then
      begin
        let ent_below = MGrid.get_at_ax ctx.grid ctx.cursor_selector#get_axial ctx.current_layer in
        if (MFaction.entity_in ent_below ctx.faction_controlled_by_player) then
          Some ctx.current_layer
        else
          raise Entity_Not_Owned_By_Player

      end
    else
      ctx.action_layer


  let set_action_dst e ctx =
    if action_type_is_set ctx && action_src_is_set ctx && MKeyboard.key_is_pressed e Sdl.Scancode.return && MAnimation.is_over ctx.animation then
      Some ctx.cursor_selector#get_axial
    else if action_cancelled e then
      None
    else
      ctx.action_dst


  let set_action_type e ctx =
    begin
      if (action_src_is_set ctx) && MAnimation.is_over ctx.animation then
        begin
          if MKeyboard.key_is_pressed e Sdl.Scancode.p then
            Some (MAction_enum.MOVE_E)
          else if MKeyboard.key_is_pressed e Sdl.Scancode.o then
            Some (MAction_enum.ATTACK_E)
          else if (MKeyboard.key_is_pressed e Sdl.Scancode.i ) then
            Some (MAction_enum.PICKUP_ITEM_E)
          else if (MKeyboard.key_is_pressed e Sdl.Scancode.u ) then
            Some (MAction_enum.USE_ITEM_E(MItem.HEALTHPACK_E))
          else if (MKeyboard.key_is_pressed e Sdl.Scancode.y ) then
            Some (MAction_enum.USE_ITEM_E(MItem.NUKE_E))
          else if action_cancelled e then
            None
          else
            ctx.action_type
        end
      else
        ctx.action_type
    end

  let action_confirmed e ctx =
    action_dst_is_set ctx && action_src_is_set ctx && MKeyboard.key_is_pressed e Sdl.Scancode.return 

  exception Unspecified_Src_Dst
  exception Unspecified_Action_Type

  exception No_healthpack

  let compute_new_grid e ctx =
    if action_confirmed e ctx then	
      match ctx.action_src,ctx.action_dst,ctx.action_layer with
      | Some src, Some dst, Some layer ->
        begin
          match ctx.action_type with
          | None -> raise Unspecified_Action_Type
          | Some x ->
            (* Determines which action is going to be executed *)
            let action =
              match x with
              | MOVE_E ->
                MAction_enum.create_move src dst layer
              | ATTACK_E -> 
                MAction_enum.create_attack src dst layer layer
              | USE_ITEM_E (item_e)-> 
                let entity = MGrid.get_at_ax ctx.grid src layer in
                let inventory = entity#get_inventory in
                let item_opt = 
                  MInventory.get_item  inventory item_e in
                let item = 
                  match item_opt with
                  | Some x -> x
                  | None -> raise No_healthpack
                in
                let param = 
                  match item_e with
                  | MItem.HEALTHPACK_E ->
                    MItem.create_healthpack_param src dst layer
                  | MItem.NUKE_E ->
                    MItem.create_nuke_param src dst layer
                in
                MAction_enum.create_use_item item#get_code param 
              | PICKUP_ITEM_E ->
                MAction_enum.create_pickup_item src dst layer
              | _ -> raise Not_yet_implemented
            in
            MAction.execute (Some action) ctx.grid 
        end
      | _ ->  raise Unspecified_Src_Dst
    else
      MAction.empty

  let check_new_turn e ctx =
    MKeyboard.key_is_pressed e Sdl.Scancode.r && is_player_turn ctx

  (* this function just gives more meaning
     when cycling through a faction list *)
  let next_faction l =
    cycle l

  exception No_faction
  let current_faction ctx = 
    match ctx.faction_list with
    | [] -> raise No_faction
    | x :: s -> x

  exception No_entities_to_play

  let compute_cpu_turn ctx =
    (* If it is not the turn of the player, we can compute the turn*)
    if not (is_player_turn ctx) && MAnimation.is_over ctx.animation then
      begin
        (* Check if the faction can play, if not, we will iterate to the next faction *)
        if (MFaction.faction_can_play (current_faction ctx)) then
          let cf = current_faction ctx in
          let units = MFaction.get_entity cf in

          (* Compute the movement for the next unit in line *)
          let rec aux l =
            match l with
            (* It shouldve been checked if the faction can play,
               not checking for this might turn into an endless loop *)
            | [] -> raise No_entities_to_play
            | x :: s ->
              if x#can_move then
                let action = MBehaviour.compute_behaviour ctx.grid x in
                MAction.execute (Some action) ctx.grid
              else
                aux s
          in

          let res = aux units in

          let faction_list =
            List.fold_right (
              fun x acc-> (MFaction.update_entities x [] (MAction.get_deleted res) ) :: acc
            ) ctx.faction_list []
          in

          {
            ctx with
            faction_list = faction_list;
            to_be_added = MAction.get_added res;
            animation = MAction.get_animation res;
            new_turn = false;
          }
        else 
          begin
            {
              ctx with
              faction_list = cycle ctx.faction_list;
              new_turn = true
            }
          end
      end
    else
      ctx

  (* Update the context in terms of frame *)
  let inc_frame ctx = 
    {
      ctx with
      frame = (ctx.frame + 1) mod 21
    }



  (* for each cpu unit we will update their behaviour *)
  let update_cpu_behaviour ctx = 
    if not (is_player_turn ctx) && MAnimation.is_over ctx.animation then
      begin
        let f = current_faction ctx in
        let res = List.fold_left ( fun acc x -> 
            let new_behaviour = MBehaviour.change_behaviour ctx.grid x in
            let res = MAction.change_behaviour ctx.grid x#get_axial x#get_lt new_behaviour in
            MAction.add res acc
          ) MAction.empty (MFaction.get_entity f)
        in
        let faction_list = 
          List.fold_right (
            fun x acc  -> let tmp = MFaction.update_entities x (MAction.get_added res) (MAction.get_deleted res) in tmp :: acc
          ) ctx.faction_list []
        in
        {
          ctx with
          faction_list = faction_list
        }
      end
    else
      ctx

  (* Update the context after event have been taken
     into account, usually this is used for animation
     or when the modificaiton on the grids are not
     done by the player *)
  let update_context_after_event ctx = 
    if MAnimation.is_over ctx.animation then
      let faction_list =
        List.fold_right (
          fun x acc-> (MFaction.update_entities x ctx.to_be_added [] ) :: acc
        ) ctx.faction_list []
      in
      {ctx with
       faction_list = faction_list;
       to_be_added = []} |> update_cpu_behaviour |> compute_cpu_turn 
    else
      ctx


  (* For each unit in the faction,
     execute the action on start *)
  let faction_on_start_actions ctx = 
    if ctx.new_turn then
      let cf = current_faction ctx in
      let units = MFaction.get_entity cf in
      let res = List.fold_left ( fun acc1 x1 -> 
          List.fold_left ( fun acc1 x2 ->
              let action = MAction_enum.action_on_start x2 x1 in
              let res = MAction.execute (Some action) ctx.grid in
              MAction.add res acc1
            ) acc1 x1#get_aos
        ) MAction.empty units
      in

      let faction_list =
        List.fold_right (
          fun x acc-> (MFaction.update_entities x [] (MAction.get_deleted res) ) :: acc
        ) ctx.faction_list []
      in

      {
        ctx with
        faction_list = faction_list;
        animation = MAction.get_animation res;
        to_be_added = MAction.get_added res;
        new_turn = false
      }
    else
      ctx

  exception Action_src_is_set_but_not_action_layer


  let get_scale e ctx =
    let offset = 
      if MKeyboard.key_is_pressed e Sdl.Scancode.z then
        0.1
      else if MKeyboard.key_is_pressed e Sdl.Scancode.x then
        -0.1
      else
        0.
    in
    ctx.scale +. offset

  exception No_interface
  (* Update the new context of the game *)
  let update_context context =
    (* let () = if context.new_turn then
        begin
          List.iter (fun x -> print_string (MFaction.to_string x)) context.faction_list;
          print_newline ();
        end
       in *)
    (* Event independant context change *)
    let ctx_before_event =
      let animation =
        MAnimation.compute_next context.animation
      in
      {
        context with
        animation = animation;
      } 
    in

    (* Get the next event in the queue *)
    let ctx_with_event = if (Sdl.poll_event ev) then
        match ev with
        (* If no event, nothing to do, except update camera position *)
        | None ->
          {
            ctx_before_event with
            camera = MCamera.update_camera context.window context.camera
          }
        (* Otherwise, check the event *)
        | Some e ->
          (* Get all the interaction of the interface *)
          let foreground_interface,background_interfaces = 
            match context.interface with
            | [] -> None,[]
            | x :: s -> Some x,s
          in
          let interface_interaction = 
            let l = match foreground_interface with
              | None -> []
              | Some i ->
                MTree.fold i
                  (
                    fun acc x -> 
                      (MInterface.fetch_interaction x e):: acc
                  )
                  []
            in
            MInterface.add_interaction l
          in

          (* Modify the foremost interface *)
          let window_interface = 
            match foreground_interface with
            | None -> None
            | Some fi ->
              let closed = match MInterface.get_close_window interface_interaction with
                | [] -> false
                | _ -> true
              in
              if closed then
                None
              else
                let window = MTree.get_elem fi in
                let window_interface = MInterface.get_interface window in
                let children_interface = MTree.get_children fi in
                let rect_window = MInterface.get_rect window_interface in
                let offset_w,offset_h = 
                  match MInterface.get_resize_window interface_interaction with
                  | None ->
                    Sdl.Rect.w rect_window,
                    Sdl.Rect.h rect_window
                  | Some x -> x
                in
                let offset_x,offset_y = 
                  match MInterface.get_move_window interface_interaction with
                  | None ->
                    Sdl.Rect.x rect_window,
                    Sdl.Rect.y rect_window
                  | Some x -> x
                in
                let new_window = 
                  let tmp1 = MInterface.set_h window_interface (Some offset_h) in
                  let tmp2 = MInterface.set_w tmp1 (Some offset_w) in
                  let tmp3 = MInterface.set_x tmp2 offset_x in
                  let tmp4 = MInterface.set_y tmp3 offset_y in
                  let tmp5 = MInterface.set_interface window tmp4 in
                  (* Update the event listeners *)
                  MInterface.set_handlers tmp5 (MInterface.get_added_handlers interface_interaction)
                in
                let res = 
                  let tmp1 = MTree.create new_window in
                  let tmp2 = MTree.append_children tmp1 children_interface in
                  Some (tmp2)
                in res
          in

          (* If the user presses escape open up a window to quit *)
          let interface = 
            match window_interface with
            | None -> 
              (* Fetch the width and height of the system window *)
              let win_w,win_h = Sdl.get_window_size ctx_before_event.window in

              (* Compute the new width and height *)
              let new_w,new_h =
                let f n q =
                  round ((float n ) *. q)
                in
                f win_w (2. /. 5.),
                f win_h (3. /. 4.)
              in

              (* Compute the new coordinates *)
              let new_x,new_y =
                MInterface.center win_w new_w,
                MInterface.center win_h new_h
              in

              (* The button for quitting *)
              let quit_button =
                let wp = 8. /. 10. in
                let h = 100 in
                let w = round (wp *. float(new_w)) in
                let x,y =
                  MInterface.center new_w w,
                  MInterface.center new_h h
                in
                MInterface.create_button  x y (Some w) (Some h) (Some wp) None "QUIT"
              in

              let new_interface_from_esc = 
                let tmp1 = MTree.create (MInterface.create_window new_x new_y (Some new_w) (Some new_h) None None) in
                let tmp2 = MTree.append_child tmp1 quit_button in
                tmp2
              in

              if MKeyboard.key_is_pressed e Sdl.Scancode.escape then
                new_interface_from_esc :: background_interfaces
              else
                background_interfaces
            | Some x -> x :: background_interfaces
          in

          (* If the user clicks the red cross button, the game closes *)
          let over = check_ev_type e Sdl.Event.quit in
          let scale = get_scale e ctx_before_event in
          let camera = get_camera e ctx_before_event.window ctx_before_event.camera scale in
          let cursor_selector_ks = {
            up = Sdl.Scancode.up;
            down = Sdl.Scancode.down;
            right = Sdl.Scancode.right;
            left = Sdl.Scancode.left;
          } in

          let cursor_selector = update_cs e ctx_before_event cursor_selector_ks in
          let action_type,action_layer,
              action_src,action_dst =
            if not (action_confirmed e ctx_before_event) then
              set_action_type e ctx_before_event,set_action_layer e ctx_before_event,
              set_action_src e ctx_before_event,set_action_dst e ctx_before_event
            else
              None,None,None,None
          in

          (* If a src is selected, display the range,
             the implementation is so that
             only when a modification
             has been made that the set of tiles
             will be computed *)
          let movement_range_selector =

            (* Main function that returns the list of the tile selected *)
            let tiles_set e1 ent_below tile_below_src tile_below_current = 
              match e1 with 
              | MAction_enum.MOVE_E ->
                MPathfinder.dijkstra_reachable tile_below_src tile_below_current context.grid ent_below#get_mp ent_below#get_lt
              | MAction_enum.PICKUP_ITEM_E ->
                MGrid.range_tile context.grid tile_below_src 1
              | MAction_enum.ATTACK_E ->
                MGrid.range_tile context.grid tile_below_src ent_below#get_ar
              | MAction_enum.USE_ITEM_E item when item = MItem.HEALTHPACK_E ->
                MGrid.range_tile context.grid tile_below_src 1 
              | MAction_enum.USE_ITEM_E item when item = MItem.NUKE_E ->
                MGrid.range_tile context.grid tile_below_src 5
              | _ -> [tile_below_src]
            in
            match context.action_src with
            | None -> 
              []
            | (Some x) ->
              begin
                let c = MCursor.create (MHex.get_r x) (MHex.get_q x) MCursor.SELECTING
                in
                let tile_below_src = MGrid.get_tile c#get_r c#get_q context.grid in
                let tile_below_current = MGrid.get_tile context.cursor_selector#get_r context.cursor_selector#get_q context.grid in
                let ent_below = 
                  match context.action_layer with
                  | None -> raise Action_src_is_set_but_not_action_layer
                  | Some layer ->
                    MGrid.get_at context.grid c#get_r c#get_q layer
                in
                match action_type,context.action_type with
                (* Action has been cancelled *)
                | None,Some e2 -> []
                (* Action has just been set *)
                | Some e1,None->
                  begin
                    tiles_set e1 ent_below tile_below_src tile_below_current
                  end
                | _,_ -> 
                  begin
                    match action_dst,context.action_dst with
                    (* action dst has just been set *)
                    | Some y1,None ->
                      begin
                        let tile_below_dst = MGrid.get_tile (MHex.get_r y1) (MHex.get_q y1) context.grid in
                        match context.action_type with
                        | None -> []
                        | Some e ->
                          begin
                            match e with
                            | MAction_enum.MOVE_E ->
                              let res,_ = MPathfinder.a_star_path_to tile_below_src tile_below_dst context.grid ent_below#get_lt in
                              res
                            | MAction_enum.ATTACK_E
                            | MAction_enum.PICKUP_ITEM_E ->
                              [tile_below_dst] 
                            | MAction_enum.USE_ITEM_E item when item = MItem.HEALTHPACK_E->
                              [tile_below_dst] 
                            | MAction_enum.USE_ITEM_E item when item = MItem.NUKE_E->
                              begin
                                (* TODO : do this better *)
                                (* Fetch the entity that has the nuke *)
                                let entity = 
                                  let layer = 
                                    match context.action_layer with
                                    | Some layer -> layer
                                    | None -> raise Exit
                                  in
                                  MGrid.get_at_ax context.grid x layer
                                in

                                let inventory = entity#get_inventory in
                                let nuke =
                                  match MInventory.get_item inventory MItem.NUKE_E with
                                  | Some nuke -> nuke
                                  | None -> raise Exit
                                in
                                let radius = MItem.get_radius_of_nuke nuke in
                                MGrid.range_tile context.grid tile_below_dst radius
                              end
                            | _ -> context.movement_range_selector
                          end
                      end
                    (* action dst has just been disabled *)
                    | None,Some y2-> 
                      begin
                        match context.action_type with
                        | None -> []
                        | Some e ->
                          begin
                            match e with
                            | MAction_enum.MOVE_E ->
                              MPathfinder.dijkstra_reachable tile_below_src tile_below_current context.grid ent_below#get_mp ent_below#get_lt
                            | MAction_enum.ATTACK_E
                            | MAction_enum.USE_ITEM_E _ 
                            | MAction_enum.PICKUP_ITEM_E ->
                              MGrid.range_tile context.grid tile_below_src ent_below#get_ar
                            | _ -> [tile_below_src]
                          end
                      end
                    | _,_ ->
                      context.movement_range_selector
                  end
              end
          in

          let added_e,deleted_e,animation_tmp = 
            let res = compute_new_grid e ctx_before_event in
            MAction.get_added res,
            MAction.get_deleted res,
            MAction.get_animation res
          in

          let faction_list,new_turn=
            let tmp = List.fold_right (
                fun x acc  -> 
                  (MFaction.update_entities x [] deleted_e ) :: acc
              ) ctx_before_event.faction_list []
            in
            if check_new_turn e ctx_before_event then
              cycle tmp,true
            else
              tmp,ctx_before_event.new_turn
          in

          let to_be_added = 
            begin
              match added_e with
              | x::s -> 
                added_e
              | [] -> 
                ctx_before_event.to_be_added
            end
          in

          let new_animation = if not (MAnimation.is_over animation_tmp) then
              animation_tmp
            else
              ctx_before_event.animation
          in
          {
            ctx_before_event with
            over = over;
            camera = camera;
            cursor_selector = cursor_selector;
            faction_list = faction_list;
            action_src = action_src;
            action_dst = action_dst;
            action_layer = action_layer;
            to_be_added = to_be_added;
            movement_range_selector = movement_range_selector;
            animation = new_animation;
            action_type = action_type;
            new_turn = new_turn;
            scale = scale;
            interface = interface;
          }
      else
        ctx_before_event
    in
    ctx_with_event |> faction_on_start_actions 
    |> update_context_after_event |> inc_frame
end
;;
