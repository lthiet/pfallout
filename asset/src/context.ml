open Keyboard_wrapper
open Utils
open Grid
open Cursor
open Faction
open Tsdl
open Hex
open Tile
open Action
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
        faction_list : MFaction.t list;
        action_src : MHex.axial_coord option;
        action_dst : MHex.axial_coord option;
        movement_range_selector : MTile.t list;
        to_be_added_m : MMilitary.t list;
        animation : MAnimation.t
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
    let update_cs e ctx ks =
        let offset = 1 in
        let tmp = ctx.cursor_selector in
        let g = ctx.grid in
        let c = tmp#set_status MCursor.SELECTING in

        if MAnimation.is_over ctx.animation then
        begin
        if check_ev_type e Sdl.Event.key_down then
            let pk = MKeyboard.get_scancode e in
            let r = new_int pk ks.down ks.up  offset c#get_r in
            let q = new_int pk ks.right ks.left  offset c#get_q in
            let tile_below = MGrid.get_tile r q g in
            if tile_below#is_impassable then
                c
            else
                c#move r q
        else
            c
        end
        else
            c#set_status MCursor.HIDDEN


    let action_src_is_set ctx =
        match ctx.action_src with
        | None -> false
        | _ -> true

    let action_dst_is_set ctx =
        match ctx.action_dst with
        | None -> false
        | _ -> true 

    let set_action_src e ctx =
        if (not (action_src_is_set ctx)) && MKeyboard.key_is_pressed e Sdl.Scancode.return && MAnimation.is_over ctx.animation then
        begin
            Some ctx.cursor_selector#get_axial
        end
        else
            ctx.action_src

    let set_action_dst e ctx =
        if action_src_is_set ctx && MKeyboard.key_is_pressed e Sdl.Scancode.return && MAnimation.is_over ctx.animation then
            Some ctx.cursor_selector#get_axial
        else
            ctx.action_dst

    let action_confirmed e ctx =
        action_dst_is_set ctx && action_src_is_set ctx && MKeyboard.key_is_pressed e Sdl.Scancode.return 

    let compute_new_grid e ctx =
        if action_confirmed e ctx then
            match ctx.action_src,ctx.action_dst with
            | Some src, Some dst ->
                MAction.move ctx.grid src dst
            | _,_ -> raise Exit
        else
            ctx.grid,[],[],(MAnimation.create [])


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
                let action_src,action_dst =
                    if not (action_confirmed e ctx_before_event) then
                        set_action_src e ctx_before_event,set_action_dst e ctx_before_event
                    else
                        None,None
                in

                (* If a src is selected, display the range *)
                let movement_range_selector =
                match action_src,context.action_src with
                | Some x,None ->
                begin
                    let c = MCursor.create (MHex.get_r x) (MHex.get_q x) MCursor.SELECTING
                    in
                    let tile_below_src = MGrid.get_tile c#get_r c#get_q context.grid in
                    let tile_below_current = MGrid.get_tile context.cursor_selector#get_r context.cursor_selector#get_q context.grid in
                    let military_below = 
                        match MGrid.get_mg_at context.grid c#get_r c#get_q with
                        | Some x -> x
                        | None -> raise Exit
                    in

                    (* Printf.printf "%d" military_below#get_mp; *)

                    match context.action_dst with
                    | None ->
                        MPathfinder.dijkstra_reachable tile_below_src tile_below_current context.grid military_below#get_mp
                    | Some y ->
                        let tile_below_dst = MGrid.get_tile (MHex.get_r y) (MHex.get_q y) context.grid in
                        let res,_ = MPathfinder.dijkstra_path tile_below_src tile_below_dst context.grid military_below#get_mp in
                        res
                end
                | None,Some x->
                    []
                | _,_ -> context.movement_range_selector

                in


                let grid,added_m,deleted_m,animation_tmp = compute_new_grid e ctx_before_event 
                in

                let () =
                print_newline ();
                match deleted_m with
                | [] -> Printf.printf "%s" "vide";
                | _ -> Printf.printf "%s" "pas vide";
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
                    animation = new_animation
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