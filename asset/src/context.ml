open Keyboard_wrapper
open Utils
open Grid
open Cursor
open Faction
open Tsdl
open Hex
open Action
open Animation
open Military

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
        animation : MAnimation.t option;
        to_be_added_m : MMilitary.t list option;
        to_be_deleted_m : MMilitary.t list option;
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

    let action_src_is_set ctx =
        match ctx.action_src with
        | None -> false
        | _ -> true

    let action_dst_is_set ctx =
        match ctx.action_dst with
        | None -> false
        | _ -> true 

    let set_action_src e ctx =
        if (not (action_src_is_set ctx)) && MKeyboard.key_is_pressed e Sdl.Scancode.return then
            Some ctx.cursor_selector#get_axial
        else
            ctx.action_src

    let set_action_dst e ctx =
        if action_src_is_set ctx && MKeyboard.key_is_pressed e Sdl.Scancode.return then
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
            ctx.grid,[],[],[]


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

                let cursor_selector = move_cursor_io e context.cursor_selector cursor_selector_ks context.grid in
                let action_src,action_dst =
                    if not (action_confirmed e context) then
                        set_action_src e context,set_action_dst e context
                    else
                        None,None
                in
                let grid,added_m,deleted_m,_ = compute_new_grid e context 
                in

                let faction_list =
                    List.fold_left (
                        fun acc x -> (MFaction.update_military x added_m deleted_m) :: acc
                    ) [] context.faction_list
                in

                {
                    context with
                    grid = grid;
                    over = over;
                    camera = camera;
                    cursor_selector = cursor_selector;
                    faction_list = faction_list;
                    action_src = action_src;
                    action_dst = action_dst;
                    to_be_added_m = Some added_m;
                    to_be_deleted_m = Some deleted_m
                }
        else
            context
end
;;