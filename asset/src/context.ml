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
open Pathfinder

let ev = Some (Sdl.Event.create ())

module MGameContext = struct
    type t = {
        over : bool;
        camera : Sdl.rect;
        grid : MGrid.t;
        cursor_selector : MCursor.cursor;
        player_turn : bool;
        new_turn : bool;
        faction_list : MFaction.t list;
        action_src : MHex.axial_coord option;
        action_dst : MHex.axial_coord option;
        movement_range_selector : MTile.t list;
        to_be_added_m : MMilitary.t list;
        animation : MAnimation.t;
		action_type : MAction_enum.t;
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

        if MAnimation.is_over ctx.animation then
        begin
        if check_ev_type e Sdl.Event.key_down then
            let pk = MKeyboard.get_scancode e in
            let r = new_int pk ks.down ks.up  offset c#get_r in
            let q = new_int pk ks.right ks.left  offset c#get_q in
            let tile_below = MGrid.get_tile r q g in
            let in_range =
                List.exists (fun x ->
                    let tmp1 = 
                        x#get_axial = tile_below#get_axial
                    in
                    let tmp2 =
                        match ctx.action_src with
                        | None -> false
                        | Some e ->
                            x#get_axial = e
                    in
                    tmp1 || tmp2
                ) ctx.movement_range_selector
            in

            if (action_src_is_set ctx && not in_range) || tile_below#is_impassable then
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
   
    let set_action_src e ctx =
        if (not (action_src_is_set ctx)) && MKeyboard.key_is_pressed e Sdl.Scancode.return && MAnimation.is_over ctx.animation then
            Some ctx.cursor_selector#get_axial
        else if action_cancelled e then
            None
        else
            ctx.action_src

    let set_action_dst e ctx =
        if action_src_is_set ctx && MKeyboard.key_is_pressed e Sdl.Scancode.return && MAnimation.is_over ctx.animation then
            Some ctx.cursor_selector#get_axial
        else if action_cancelled e then
            None
        else
            ctx.action_dst
	
	let set_action_type e ctx =
    (* TODO: factorize *)
		if (not (action_src_is_set ctx)) && MKeyboard.key_is_pressed e Sdl.Scancode.m && MAnimation.is_over ctx.animation then
			MAction_enum.MOVE
		else if (not (action_src_is_set ctx)) && MKeyboard.key_is_pressed e Sdl.Scancode.o && MAnimation.is_over ctx.animation then
			MAction_enum.ATTACK
		else if (not (action_src_is_set ctx)) && action_cancelled e && MAnimation.is_over ctx.animation then
			MAction_enum.NOTHING
		else 
			ctx.action_type
	
    let action_confirmed e ctx =
        action_dst_is_set ctx && action_src_is_set ctx && MKeyboard.key_is_pressed e Sdl.Scancode.return 

    let compute_new_grid e ctx =
        if action_confirmed e ctx then	
            match ctx.action_src,ctx.action_dst
			,ctx.action_type with
            | Some src, Some dst, MAction_enum.MOVE  ->
                MAction.move ctx.grid src dst
			| Some src, Some dst, MAction_enum.ATTACK ->
				MAction.attack ctx.grid src dst	
			| Some src, Some dst, MAction_enum.NOTHING -> raise Nothing
            | _,_,_ ->  raise Exit
			
			
        else
            ctx.grid,[],[],(MAnimation.create []),MAction_enum.MOVE 

    let new_turn e =
        MKeyboard.key_is_pressed e Sdl.Scancode.f1

    (* Update the new context of the game *)
    let update_context context =
        (* Event independant context change *)
        let ctx_before_event =
            let animation =
                MAnimation.compute_next context.animation
            in
            {
                context with
                animation = animation
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
                let 
				keyboard_action_type, 
				action_src,action_dst =
                    if not (action_confirmed e ctx_before_event) then
                        set_action_type e ctx_before_event,
						set_action_src e ctx_before_event,set_action_dst e ctx_before_event
                    else
                        context.action_type,
						None,None
                in

                (* If a src is selected, display the range,
                the implementation is so that
                only when a modification
                has been made that the set of tiles
                will be computed *)
                let movement_range_selector =
                    match action_src with
                    | None -> []
                    | Some x1 ->
                        let c = MCursor.create (MHex.get_r x1) (MHex.get_q x1) MCursor.SELECTING
                        in
                        let tile_below_src = MGrid.get_tile c#get_r c#get_q context.grid in
                        let tile_below_current = MGrid.get_tile context.cursor_selector#get_r context.cursor_selector#get_q context.grid in
                        let military_below = 
                            match MGrid.get_mg_at context.grid c#get_r c#get_q with
                            | Some x -> x
                            | None -> raise Exit
                        in
                        match context.action_src with
                        | None ->
                            begin
                            match context.action_type with
                            | MOVE ->
                                MPathfinder.dijkstra_reachable tile_below_src tile_below_current context.grid military_below#get_mp
                            | ATTACK ->
                                MGrid.range_tile context.grid tile_below_src military_below#get_ar
                            | _ -> 
                                []
                            end
                        | Some x2 ->
                            match action_dst,context.action_dst with
                            (* action dst has just been set *)
                            | Some y1,None ->
                                let tile_below_dst = MGrid.get_tile (MHex.get_r y1) (MHex.get_q y1) context.grid in
                                let res,_ = MPathfinder.dijkstra_path tile_below_src tile_below_dst context.grid military_below#get_mp in
                                res
                            (* action dst has just been disabled *)
                            | None,Some y2-> 
                            begin
                                match context.action_type with
                                | MOVE ->
                                    MPathfinder.dijkstra_reachable tile_below_src tile_below_current context.grid military_below#get_mp
                                | ATTACK ->
                                    MGrid.range_tile context.grid tile_below_src military_below#get_ar
                                | _ -> 
                                    []
                            end
                            | _,_ ->
                                context.movement_range_selector
                in

                let grid,added_m,deleted_m,animation_tmp,compute_action_type = compute_new_grid e ctx_before_event 
                in
				
				let new_action_type = match compute_action_type with
					| MAction_enum.NOTHING -> compute_action_type
					| _ -> keyboard_action_type
                in

                let faction_list =
                    List.fold_left (
                        fun acc x -> 
                        (MFaction.update_military x [] deleted_m ) :: acc
                    ) [] ctx_before_event.faction_list;
                in

                let to_be_added_m = 
                    begin
                        match added_m with
                        | x::s -> 
                            added_m
                        | [] -> 
                            ctx_before_event.to_be_added_m
                    end
                in

                let new_animation = if not (MAnimation.is_over animation_tmp) then
                        animation_tmp
                    else
                        ctx_before_event.animation
                in

                {
                    ctx_before_event with
                    grid = grid;
                    over = over;
                    camera = camera;
                    cursor_selector = cursor_selector;
                    faction_list = faction_list;
                    action_src = action_src;
                    action_dst = action_dst;
                    to_be_added_m = to_be_added_m;
                    movement_range_selector = movement_range_selector;
                    animation = new_animation;
					action_type = new_action_type
                }
        else
            ctx_before_event
        in

        let ctx_after_event = if MAnimation.is_over ctx_with_event.animation then
            let faction_list =
                List.fold_left (
                    fun acc x -> (MFaction.update_military x ctx_with_event.to_be_added_m [] ) :: acc
                ) [] ctx_with_event.faction_list;
            in
            let to_be_added_m = [] in
            {ctx_with_event with
            faction_list = faction_list;
            to_be_added_m = to_be_added_m}
        else
            ctx_with_event
        in
        ctx_after_event
end
;;
