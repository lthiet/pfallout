open Tree
open Tsdl
open Utils
open Texture_pack
open Texture_wrapper
open Keyboard_wrapper
open Mouse_wrapper

(* Implements the interface of the game *)
(* All interface textures must have a corner, top, left and center texture, they may also have multiple state if needed *)
(* Some interface may feature a different corner for bottom or right, as well as bottom border and right texture *)

(* Each interface has some event that it listens to, when "collided" with an event, it will return an interact product type *)
module MInterface = struct
  type kind = 
    (* Simple is just one image that is rendered completely*)
    | SIMPLE
    (* Composed has mutiple part that compose its image, ie corner, top, left etc... *)
    | COMPOSED

  type role = 
    | BUTTON
    | WINDOW


  (* A single element from the interface *)
  type interface = {
    (* All the coordinates are relative to the parent *)
    x : int; 
    y : int;
    w : int;
    h : int;
    kind : kind;
    role : role;
  }
  type interaction = 
    {
      resize_window : int * int;
      close_window : int list;
      move_window : (int * int) option;
      handlers : handler list
    }
  and handler = (Sdl.event -> interface -> interaction)

  let set_resize_window interaction x = {interaction with resize_window = x}
  let set_close_window interaction x = {interaction with close_window = x}
  let set_move_window interaction x = {interaction with move_window = x}
  let empty_interaction =
    {
      resize_window = 0,0;
      close_window = [];
      move_window = None;
      handlers = [];
    }

  let get_resize_window t = t.resize_window
  let get_close_window t = t.close_window
  let get_move_window t = t.move_window
  let get_added_handlers t = t.handlers

  let add_interaction l =
    List.fold_left (
      fun acc x ->
        let old_w,old_h = acc.resize_window in
        let new_w,new_h = x.resize_window in
        let old_closed_window = acc.close_window in
        let new_closed_window = x.close_window in
        let old_handlers = acc.handlers in
        let new_handlers = x.handlers in
        let new_move_window = match acc.move_window,x.move_window with
          | None, None -> None
          | Some (a,b), None -> Some(a,b)
          | None, Some (a,b) -> Some(a,b)
          | Some (a,b),Some(c,d) -> Some(a+c,b+d)
        in
        {
          resize_window = (old_w + new_w),(old_h + new_h);
          close_window = new_closed_window @ old_closed_window;
          move_window = new_move_window;
          handlers =  old_handlers @ new_handlers;
        }
    ) empty_interaction l

  let set_x i e = { i with x = e }
  let set_y i e = { i with y = e }
  let set_w i e = { i with w = e }
  let set_h i e = { i with h = e }

  let get_rect i = Sdl.Rect.create i.x i.y i.w i.h


  type t = {
    interface : interface;
    handlers : handler list;
  }

  let get_interface t = t.interface
  let get_handlers t = t.handlers
  let set_interface t e = { t with interface = e}
  let set_handlers t e = { t with handlers = e}


  let modify t x y w h =
    {
      t with
      x = x;
      y = y;
      w = w;
      h = h;
    }

  let incr_w t x =
    {
      t with
      w = t.w + x
    }

  let incr_h t x =
    {
      t with
      h = t.h + x
    }


  let incr_x t x =
    {
      t with
      x = t.x + x
    }


  let incr_y t x =
    {
      t with
      y = t.y + x
    }

  let create_window x y w h =
    let interface = {
      x = x;
      y = y;
      w = w;
      h = h;
      kind = COMPOSED;
      role = WINDOW;
    } in

    (* Change the size of the window when input is YHGJ *)
    let rec f = (
      fun ev interface -> 
        let w,h =
          if MKeyboard.get_scancode ev = Sdl.Scancode.g then
            (-10,0)
          else if MKeyboard.get_scancode ev = Sdl.Scancode.j then
            (10,0)
          else if MKeyboard.get_scancode ev = Sdl.Scancode.y then
            (0,-10)
          else if MKeyboard.get_scancode ev = Sdl.Scancode.h then
            (0,10)
          else
            (0,0)
        in
        {
          empty_interaction with
          resize_window = w,h;
          handlers = [f]
        }
    ) 
    in

    let rec f2 = (
      fun ev interface_init ->
        if not (check_ev_type ev Sdl.Event.mouse_button_down) || not (MMouse.is_inside ev (interface_init.x) (interface_init.y-200) interface_init.w (200)) then
          {empty_interaction with
           handlers = [f2]}
        else 
          let _,(mx,my) = Sdl.get_mouse_state () in
          let rec fbis = (
            fun ev interface -> 
              if check_ev_type ev Sdl.Event.mouse_button_up then
                {
                  empty_interaction with
                  handlers = [f2]
                }
              else
                let _,(new_x,new_y) = Sdl.get_mouse_state () in
                {
                  empty_interaction with
                  move_window = Some (interface_init.x+(new_x-mx),interface_init.y+(new_y-my));
                  handlers = [fbis]
                }
          )
          in
          {
            empty_interaction with
            handlers = [fbis]
          }
    )
    in
    let handlers = 
      [f;f2]
    in
    {
      interface = interface;
      handlers = handlers
    }

  let fetch_interaction t ev =
    let interface = t.interface in
    let l = List.fold_left 
        ( fun acc x -> 
            (* if check_ev_type ev x.event then *)
            (x ev interface) :: acc
            (* else 
               {empty_interaction with event_listeners = [x]} :: acc *)
        )
        [] t.handlers
    in add_interaction l

  (* Display part *)
  type rects = {
    corner_top_left: Sdl.rect;
    horizontal_top : Sdl.rect;
    vertical_left : Sdl.rect;
    center : Sdl.rect;
  }

  let window_rect = {
    corner_top_left = Sdl.Rect.create 0 0 200 200;
    horizontal_top = Sdl.Rect.create 200 0 200 200;
    vertical_left = Sdl.Rect.create 0 200 200 200;
    center = Sdl.Rect.create 200 200 200 200;
  }

  let match_role_to_rect role =
    match role with
    | WINDOW -> window_rect
    | BUTTON -> raise Not_yet_implemented


  let render renderer interface textures =
    let txt = MTexture_pack.get_ui textures in
    let rects = match_role_to_rect interface.role in

    (* Render the corners *)
    (* Top left *)
    MTexture.render renderer ~clip_src:(Some rects.corner_top_left) ~x:(interface.x-200) ~y:(interface.y-200) txt;
    (* Top right *)
    MTexture.render renderer ~clip_src:(Some rects.corner_top_left) ~x:(interface.x+interface.w) ~y:(interface.y-200) ~flip:Sdl.Flip.horizontal txt;
    (* Bottom left *)
    MTexture.render renderer ~clip_src:(Some rects.corner_top_left) ~x:(interface.x-200) ~y:(interface.y+interface.h) ~flip:Sdl.Flip.vertical txt;
    (* Bottom right *)
    MTexture.render renderer ~clip_src:(Some rects.corner_top_left) ~x:(interface.x+interface.w) ~y:(interface.y+interface.h) ~flip:(Sdl.Flip.(+) Sdl.Flip.vertical Sdl.Flip.horizontal) txt;

    (* Render the horizontal bars *)
    let nb_horizontal_bar,horizontal_offset = division_eclid interface.w 200 in
    let rect_horizontal_offset = 
      Sdl.Rect.create (Sdl.Rect.x rects.horizontal_top) (Sdl.Rect.y rects.horizontal_top) (horizontal_offset) (Sdl.Rect.h rects.horizontal_top)
    in
    let () = for i = 0 to nb_horizontal_bar-1 do 
        MTexture.render renderer ~clip_src:(Some rects.horizontal_top) ~x:(interface.x+200*i) ~y:(interface.y-200) txt;
        MTexture.render renderer ~clip_src:(Some rects.horizontal_top) ~x:(interface.x+200*i) ~y:(interface.y+interface.h) ~flip:Sdl.Flip.vertical txt;
      done;
      if horizontal_offset > 0 then
        begin
          MTexture.render renderer ~clip_src:(Some rect_horizontal_offset) ~x:(interface.x+200*nb_horizontal_bar) ~y:(interface.y-200) txt;
          MTexture.render renderer ~clip_src:(Some rect_horizontal_offset) ~x:(interface.x+200*nb_horizontal_bar) ~y:(interface.y+interface.h) ~flip:Sdl.Flip.vertical txt;
        end


    in

    (* Render the vertical bars *)
    let nb_vertical_bar,vertical_offset = division_eclid interface.h 200 in

    let rect_vertical_offset = 
      Sdl.Rect.create (Sdl.Rect.x rects.vertical_left) (Sdl.Rect.y rects.vertical_left) (Sdl.Rect.w rects.vertical_left) (vertical_offset)
    in
    let () = for i = 0 to nb_vertical_bar-1 do 
        MTexture.render renderer ~clip_src:(Some rects.vertical_left) ~x:(interface.x-200) ~y:(interface.y+i*200) txt;
        MTexture.render renderer ~clip_src:(Some rects.vertical_left) ~x:(interface.x+interface.w) ~y:(interface.y+i*200) ~flip:Sdl.Flip.horizontal txt;
      done;
      if vertical_offset > 0 then
        begin
          MTexture.render renderer ~clip_src:(Some rect_vertical_offset) ~x:(interface.x-200) ~y:(interface.y+ nb_vertical_bar * 200) txt;
          MTexture.render renderer ~clip_src:(Some rect_vertical_offset) ~x:(interface.x+interface.w) ~y:(interface.y+ nb_vertical_bar * 200) ~flip:Sdl.Flip.horizontal txt;
        end
    in

    let center_rect_vertical_offset =
      let x,y,w,h =
        Sdl.Rect.x rect_vertical_offset,
        Sdl.Rect.y rect_vertical_offset,
        Sdl.Rect.w rect_vertical_offset,
        Sdl.Rect.h rect_vertical_offset
      in
      Sdl.Rect.create (x+200) (y) w h
    in

    let center_rect_horizontal_offset =
      let x,y,w,h =
        Sdl.Rect.x rect_horizontal_offset,
        Sdl.Rect.y rect_horizontal_offset,
        Sdl.Rect.w rect_horizontal_offset,
        Sdl.Rect.h rect_horizontal_offset
      in
      Sdl.Rect.create (x) (y+200) w h
    in



    (* Render the center *)
    let () = 
      for i = 0 to nb_horizontal_bar-1 do
        for j = 0 to nb_vertical_bar-1 do
          MTexture.render renderer ~clip_src:(Some rects.center) ~x:(interface.x+200*i) ~y:(interface.y+200*j) ~flip:Sdl.Flip.horizontal txt;
        done;
      done;


      (* Render the offset *)
      if vertical_offset > 0 then
        for i = 0 to nb_horizontal_bar-1 do
          begin
            MTexture.render renderer ~clip_src:(Some center_rect_vertical_offset) ~x:(interface.x+i*200) ~y:(interface.y+200*nb_vertical_bar) txt;
          end;
        done;

      if horizontal_offset > 0 then
        begin
          for i = 0 to nb_vertical_bar-1 do
            MTexture.render renderer ~clip_src:(Some center_rect_horizontal_offset) ~x:(interface.x+200*nb_horizontal_bar) ~y:(interface.y+200*i) txt;
          done;
        end;
    in

    if vertical_offset > 0 && horizontal_offset > 0 then
      begin
        match Sdl.intersect_rect center_rect_vertical_offset center_rect_horizontal_offset with
        | None -> ()
        | Some r ->
          MTexture.render renderer ~clip_src:(Some r) ~x:(interface.x+200*nb_horizontal_bar) ~y:(interface.y+200*nb_vertical_bar) txt
      end

  (* The whole interface currently displayed,
     only the interface at the top of the list can
     be interacted with, the rest is only displayed *)
  type structure = t MTree.tree list

  let render_struct renderer interface_struct textures =
    List.iter (fun x ->
        MTree.iter x (fun y ->
            render renderer y.interface textures
          )
      ) interface_struct;
end
