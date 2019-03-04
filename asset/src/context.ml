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
open Entity
open Pathfinder
open Behaviour
open Camera

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
    action_type : MAction_enum.t option;
    movement_range_selector : MTile.t list;
    to_be_added : MEntity.t list;
    to_be_deleted : MEntity.t list;
    animation : MAnimation.t;
    new_turn : bool;
    frame : int
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
  let get_camera e c =
    MCamera.change_direction c e |> MCamera.update_camera

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
        let ent_below = MGrid.get_mg_at_ax ctx.grid ctx.cursor_selector#get_axial in
        if (MFaction.entity_in ent_below ctx.faction_controlled_by_player) then
          Some ctx.cursor_selector#get_axial
        else
          raise Entity_Not_Owned_By_Player
      end
    else if action_cancelled e then
      None
    else
      ctx.action_src

  let set_action_dst e ctx =
    if action_type_is_set ctx && action_src_is_set ctx && MKeyboard.key_is_pressed e Sdl.Scancode.return && MAnimation.is_over ctx.animation then
      Some ctx.cursor_selector#get_axial
    else if action_cancelled e then
      None
    else
      ctx.action_dst

  (* Check whether of not the key for refilling mp
     has been pressed *)
  let refill_mp_kp e =
    MKeyboard.key_is_pressed e Sdl.Scancode.i

  let set_action_type e ctx =
    begin
      if (action_src_is_set ctx) && MAnimation.is_over ctx.animation then
        begin
          if MKeyboard.key_is_pressed e Sdl.Scancode.p then
            Some (MAction_enum.MOVE)
          else if MKeyboard.key_is_pressed e Sdl.Scancode.o then
            Some (MAction_enum.ATTACK)
          else if refill_mp_kp e then
            Some (MAction_enum.REFILL_MP)
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

  let compute_new_grid e ctx =
    if action_confirmed e ctx then	
      match ctx.action_src,ctx.action_dst with
      | Some src, Some dst ->
        begin
          match ctx.action_type with
          | None -> raise Unspecified_Action_Type
          | Some x ->
            let action = MAction.create x src dst in
            MAction.execute (Some action) ctx.grid 
        end
      | _,_ ->  raise Unspecified_Src_Dst
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
            let res = MAction.change_behaviour ctx.grid x#get_axial new_behaviour in
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
              let action = MAction.create x2 x1#get_axial x1#get_axial in
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


  (* Update the new context of the game *)
  let update_context context =
    (* List.iter (fun x -> print_string (MFaction.to_string x)) context.faction_list;
       print_newline (); *)
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
        (* If no event, nothing to do *)
        | None ->
          ctx_before_event
        (* Otherwise, check the event *)
        | Some e ->
          (* If the user clicks the red cross button, the game closes *)
          let over = check_ev_type e Sdl.Event.quit in
          let camera = get_camera e ctx_before_event.camera in
          let cursor_selector_ks = {
            up = Sdl.Scancode.up;
            down = Sdl.Scancode.down;
            right = Sdl.Scancode.right;
            left = Sdl.Scancode.left;
          } in

          let cursor_selector = update_cs e ctx_before_event cursor_selector_ks in
          let action_type, 
              action_src,action_dst =
            if not (action_confirmed e ctx_before_event) then
              set_action_type e ctx_before_event,
              set_action_src e ctx_before_event,set_action_dst e ctx_before_event
            else
              None,None,None
          in

          (* If a src is selected, display the range,
             the implementation is so that
             only when a modification
             has been made that the set of tiles
             will be computed *)
          let movement_range_selector =
            match context.action_src with
            | None -> 
              []
            | Some x ->
              begin
                let c = MCursor.create (MHex.get_r x) (MHex.get_q x) MCursor.SELECTING
                in
                let tile_below_src = MGrid.get_tile c#get_r c#get_q context.grid in
                let tile_below_current = MGrid.get_tile context.cursor_selector#get_r context.cursor_selector#get_q context.grid in
                let ent_below = MGrid.get_mg_at context.grid c#get_r c#get_q in
                match action_type,context.action_type with
                (* Action has been cancelled *)
                | None,Some e2 -> []
                (* Action has just been set *)
                | Some e1,None->
                  begin
                    match e1 with 
                    | MAction_enum.MOVE ->
                      MPathfinder.dijkstra_reachable tile_below_src tile_below_current context.grid ent_below#get_current_mp
                    | MAction_enum.ATTACK ->
                      MGrid.range_tile context.grid tile_below_src ent_below#get_ar
                    | _ -> [tile_below_src]
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
                            | MAction_enum.MOVE ->
                              let res,_ = MPathfinder.dijkstra_path tile_below_src tile_below_dst context.grid ent_below#get_current_mp in
                              res
                            | MAction_enum.ATTACK ->
                              [tile_below_dst] 
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
                            | MAction_enum.MOVE ->
                              MPathfinder.dijkstra_reachable tile_below_src tile_below_current context.grid ent_below#get_current_mp
                            | MAction_enum.ATTACK ->
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
            to_be_added = to_be_added;
            movement_range_selector = movement_range_selector;
            animation = new_animation;
            action_type = action_type;
            new_turn = new_turn
          }
      else
        ctx_before_event
    in
    ctx_with_event |> faction_on_start_actions 
    |> update_context_after_event |> inc_frame
end
;;
