open Tree
open Tsdl
open Utils
open Texture_pack
open Texture_wrapper
open Keyboard_wrapper

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

  type event_param =
    | KEY_DOWN of Sdl.scancode
    | NONE

  type interaction =
    (* The width and height offset to change the main window *)
    | RESIZE_WINDOW of int * int
    (* The ID of the window to close, possibly not needed *)
    | CLOSE_WINDOW of int 

  type event_listener = 
    {
      event_param : event_param;
      interaction : interaction
    }

  type interaction_result = 
    {
      resize_window : int * int;
      close_window : int list
    }

  let get_resize_window t = t.resize_window
  let get_close_window t = t.close_window

  let add_interaction l =
    let init =
      {
        resize_window = 0,0;
        close_window = []
      }
    in
    List.fold_left (
      fun acc x ->
        match x with
        | RESIZE_WINDOW (w,h) ->
          let old_w,old_h = acc.resize_window in
          {acc with
           resize_window = (old_w+w,old_h+h)}
        | CLOSE_WINDOW id ->
          {acc with
           close_window = id :: acc.close_window}
    ) init l


  (* A single element from the interface *)
  type t = {
    (* All the coordinates are relative to the parent *)
    x : int; 
    y : int;
    w : int;
    h : int;
    kind : kind;
    role : role;
    event_listeners : event_listener list
  }

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
    {
      x = x;
      y = y;
      w = w;
      h = h;
      kind = COMPOSED;
      role = WINDOW;
      event_listeners = [
        {
          event_param = KEY_DOWN(Sdl.Scancode.y);
          interaction = RESIZE_WINDOW(0,-10)
        };
        {
          event_param = KEY_DOWN(Sdl.Scancode.h);
          interaction = RESIZE_WINDOW(0,10)
        };
        {
          event_param = KEY_DOWN(Sdl.Scancode.j);
          interaction = RESIZE_WINDOW(10,0)
        };
        {
          event_param = KEY_DOWN(Sdl.Scancode.g);
          interaction = RESIZE_WINDOW(-10,0)
        }
      ]
    }

  let check_event_listener ev_param ev =
    match ev_param with
    | KEY_DOWN(key) ->
      MKeyboard.key_is_pressed ev key
    | NONE -> false
    | _ -> raise Not_yet_implemented

  (* Interaction that may come from window : resize using the YHGJ keys *)
  let fetch_interaction_window window ev =
    List.fold_left 
      ( fun acc x -> 
          let event_param = x.event_param in
          if not (check_event_listener event_param ev) then
            acc
          else
            x.interaction :: acc
      )
      [] window.event_listeners

  let fetch_interaction t ev =
    match t.role with
    | WINDOW -> fetch_interaction_window t ev
    | _ -> []






















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
            render renderer y textures
          )
      ) interface_struct;
end
