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
    w : int option;
    w_percent : float option;
    h : int option;
    h_percent : float option;
    text : string option;
    kind : kind;
    role : role;
  }
  type interaction = 
    {
      resize_window : (int * int) option;
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
      resize_window = None;
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
        let new_resize_window = match acc.resize_window,x.resize_window with
          | None, None -> None
          | Some (a,b), None -> Some(a,b)
          | None, Some (a,b) -> Some(a,b)
          | Some (a,b),Some(c,d) -> Some(a+c,b+d)
        in
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
          resize_window = new_resize_window;
          close_window = new_closed_window @ old_closed_window;
          move_window = new_move_window;
          handlers =  old_handlers @ new_handlers;
        }
    ) empty_interaction l

  let set_x i e = { i with x = e }
  let set_y i e = { i with y = e }
  let set_w i e = { i with w = e }
  let set_h i e = { i with h = e }

  let int_option_matcher x =
    match x with
    | None -> 0
    | Some x -> x

  let get_rect i = 
    let w = int_option_matcher i.w in
    let h = int_option_matcher i.h in

    Sdl.Rect.create i.x i.y w h 


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
      w = Some((int_option_matcher t.w) + x)
    }

  let incr_h t x =
    {
      t with
      h = Some ((int_option_matcher t.h) + x)
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

  let create_button x y w h w_percent h_percent text =
    let interface = {
      x = x;
      y = y;
      w = w;
      h = h;
      w_percent = w_percent;
      h_percent = h_percent;
      text = Some text;
      kind = COMPOSED;
      role = BUTTON;
    }
    in 
    {
      interface = interface;
      handlers = []
    }

  let create_window x y w h w_percent h_percent =
    let interface = {
      x = x;
      y = y;
      w = w;
      h = h;
      w_percent = w_percent;
      h_percent = h_percent;
      text = None;
      kind = COMPOSED;
      role = WINDOW;
    } in

    let rec close = (
      fun ev interface_init ->
        let rect_init = get_rect interface_init in
        let _,(mx,my) = Sdl.get_mouse_state () in
        let x_init,y_init,w_init,h_init =
          Sdl.Rect.x rect_init,
          Sdl.Rect.y rect_init,
          Sdl.Rect.w rect_init,
          Sdl.Rect.h rect_init
        in
        let zone_to_look = 
          Sdl.Rect.create (x_init + w_init) (y_init - 200) 200 200
        in
        let mouse_point = Sdl.Point.create mx my in
        if
          not (
            check_ev_type ev Sdl.Event.mouse_button_down
          ) || 
          not (Sdl.point_in_rect mouse_point zone_to_look)
        then
          {empty_interaction with
           handlers = [close]}
        else
          {
            empty_interaction with
            close_window = [-1];
          }
    )
    in

    let rec drag_resize = (
      fun ev interface_init ->
        let rect_init = get_rect interface_init in
        let _,(mx,my) = Sdl.get_mouse_state () in
        let x_init,y_init,w_init,h_init =
          Sdl.Rect.x rect_init,
          Sdl.Rect.y rect_init,
          Sdl.Rect.w rect_init,
          Sdl.Rect.h rect_init
        in
        let zone_to_look = 
          Sdl.Rect.create (x_init + w_init - 200) (y_init + h_init - 200) 200 200
        in
        let mouse_point = Sdl.Point.create mx my in
        if
          not (
            check_ev_type ev Sdl.Event.mouse_button_down
          ) || 
          not (Sdl.point_in_rect mouse_point zone_to_look)
        then
          {empty_interaction with
           handlers = [drag_resize]}
        else
          let _,(mx,my) = Sdl.get_mouse_state () in
          let rec fbis = (
            fun ev interface ->
              if check_ev_type ev Sdl.Event.mouse_button_up then
                {
                  empty_interaction with
                  handlers = [drag_resize]
                }
              else
                let _,(new_mx,new_my) = Sdl.get_mouse_state () in
                (* TODO : elagantize this *)
                let new_x,new_y,new_w,new_h =
                  interface.x,interface.y,
                  (-(mx-new_mx) + (int_option_matcher interface_init.w)),(-(my-new_my) +(int_option_matcher interface_init.h))
                in
                {
                  empty_interaction with
                  resize_window = Some (max 200 new_w, max 200 new_h);
                  move_window = Some (new_x,new_y);
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

    let rec drag_position = (
      fun ev interface_init ->
        if not (check_ev_type ev Sdl.Event.mouse_button_down) || not (MMouse.is_inside ev (interface_init.x) (interface_init.y-200) (int_option_matcher interface_init.w) (200)) then
          {empty_interaction with
           handlers = [drag_position]}
        else 
          let _,(mx,my) = Sdl.get_mouse_state () in
          let rec fbis = (
            fun ev interface -> 
              if check_ev_type ev Sdl.Event.mouse_button_up then
                {
                  empty_interaction with
                  handlers = [drag_position]
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
      [drag_position;drag_resize;close]
    in
    {
      interface = interface;
      handlers = handlers
    }

  let fetch_interaction t ev =
    let interface = t.interface in
    let l = List.fold_left 
        ( fun acc x -> 
            (x ev interface) :: acc
        )
        [] t.handlers
    in add_interaction l

  (* Display part *)
  type rects = {
    corner_top_left: Sdl.rect;
    horizontal_top : Sdl.rect;
    vertical_left : Sdl.rect;
    center : Sdl.rect;
    close : Sdl.rect option;
    resize : Sdl.rect option;
  }

  let window_rect = {
    corner_top_left = Sdl.Rect.create 0 0 200 200;
    horizontal_top = Sdl.Rect.create 200 0 200 200;
    vertical_left = Sdl.Rect.create 0 200 200 200;
    center = Sdl.Rect.create 200 200 200 200;
    close = Some (Sdl.Rect.create 400 0 200 200);
    resize = Some (Sdl.Rect.create 400 200 200 200);
  }
  let button_rect = {
    corner_top_left = Sdl.Rect.create 0 0 50 50;
    horizontal_top = Sdl.Rect.create 50 0 50 50;
    vertical_left = Sdl.Rect.create 0 50 50 50;
    center = Sdl.Rect.create 50 50 50 50;
    close = None;
    resize = None;
  }

  let match_role_to_rect role =
    match role with
    | WINDOW -> window_rect
    | BUTTON -> button_rect

  let match_role_to_txt role textures =
    match role with
    | WINDOW -> MTexture_pack.get_ui textures
    | BUTTON -> MTexture_pack.get_ui_button textures

  let match_role_to_offset role =
    match role with
    | WINDOW -> 200
    | BUTTON -> 50

  let set_to_relative_coordinate tree =
    MTree.map tree ( fun x -> x ) ( fun x -> 
        let interface = x.interface in
        let px,py =
          interface.x,
          interface.y
        in
        (fun y -> {
             y with
             interface = {
               y.interface with
               x = y.interface.x + px;
               y = y.interface.y + py;
             }
           }
        )
      )

  let set_to_relative_size tree =
    MTree.map tree ( fun x -> x ) ( fun x -> 
        let interface = x.interface in
        let pw,ph =
          int_option_matcher interface.w,
          int_option_matcher interface.h
        in
        (fun y -> 
           let new_w = 
             match y.interface.w_percent with
             | None -> int_option_matcher y.interface.w
             | Some percent -> round (float(pw) *. percent)
           in
           let new_h = 
             match y.interface.h_percent with
             | None -> int_option_matcher y.interface.h
             | Some percent -> round (float(ph) *. percent)
           in
           {
             y with
             interface = {
               y.interface with
               w = Some new_w;
               h = Some new_h
             }
           }
        )
      )




  let render renderer interface textures =
    let txt = match_role_to_txt interface.role textures in
    let rects = match_role_to_rect interface.role in
    let offset = match_role_to_offset interface.role in

    let interface_w,interface_h =
      int_option_matcher interface.w,
      int_option_matcher interface.h
    in

    (* Render the corners *)
    (* Top left *)
    MTexture.render renderer ~clip_src:(Some rects.corner_top_left) ~x:(interface.x-offset) ~y:(interface.y-offset) txt;
    (* Top right *)
    MTexture.render renderer ~clip_src:(Some rects.corner_top_left) ~x:(interface.x+interface_w) ~y:(interface.y-offset) ~flip:Sdl.Flip.horizontal txt;
    (* Close button *)
    let () = 
      match rects.close with
      | None -> ()
      | Some _ ->
        MTexture.render renderer ~clip_src:(rects.close) ~x:(interface.x+interface_w) ~y:(interface.y-offset) ~flip:Sdl.Flip.horizontal txt;
    in
    (* Bottom left *)
    MTexture.render renderer ~clip_src:(Some rects.corner_top_left) ~x:(interface.x-offset) ~y:(interface.y+interface_h) ~flip:Sdl.Flip.vertical txt;
    (* Bottom right *)
    MTexture.render renderer ~clip_src:(Some rects.corner_top_left) ~x:(interface.x+interface_w) ~y:(interface.y+interface_h) ~flip:(Sdl.Flip.(+) Sdl.Flip.vertical Sdl.Flip.horizontal) txt;


    (* Render the horizontal bars *)
    let nb_horizontal_bar,horizontal_offset = division_eclid interface_w offset in
    let rect_horizontal_offset = 
      Sdl.Rect.create (Sdl.Rect.x rects.horizontal_top) (Sdl.Rect.y rects.horizontal_top) (horizontal_offset) (Sdl.Rect.h rects.horizontal_top)
    in
    let () = for i = 0 to nb_horizontal_bar-1 do 
        MTexture.render renderer ~clip_src:(Some rects.horizontal_top) ~x:(interface.x+offset*i) ~y:(interface.y-offset) txt;
        MTexture.render renderer ~clip_src:(Some rects.horizontal_top) ~x:(interface.x+offset*i) ~y:(interface.y+interface_h) ~flip:Sdl.Flip.vertical txt;
      done;
      if horizontal_offset > 0 then
        begin
          MTexture.render renderer ~clip_src:(Some rect_horizontal_offset) ~x:(interface.x+offset*nb_horizontal_bar) ~y:(interface.y-offset) txt;
          MTexture.render renderer ~clip_src:(Some rect_horizontal_offset) ~x:(interface.x+offset*nb_horizontal_bar) ~y:(interface.y+interface_h) ~flip:Sdl.Flip.vertical txt;
        end


    in

    (* Render the vertical bars *)
    let nb_vertical_bar,vertical_offset = division_eclid interface_h offset in

    let rect_vertical_offset = 
      Sdl.Rect.create (Sdl.Rect.x rects.vertical_left) (Sdl.Rect.y rects.vertical_left) (Sdl.Rect.w rects.vertical_left) (vertical_offset)
    in
    let () = for i = 0 to nb_vertical_bar-1 do 
        MTexture.render renderer ~clip_src:(Some rects.vertical_left) ~x:(interface.x-offset) ~y:(interface.y+i*offset) txt;
        MTexture.render renderer ~clip_src:(Some rects.vertical_left) ~x:(interface.x+interface_w) ~y:(interface.y+i*offset) ~flip:Sdl.Flip.horizontal txt;
      done;
      if vertical_offset > 0 then
        begin
          MTexture.render renderer ~clip_src:(Some rect_vertical_offset) ~x:(interface.x-offset) ~y:(interface.y+ nb_vertical_bar * offset) txt;
          MTexture.render renderer ~clip_src:(Some rect_vertical_offset) ~x:(interface.x+interface_w) ~y:(interface.y+ nb_vertical_bar * offset) ~flip:Sdl.Flip.horizontal txt;
        end
    in

    let center_rect_vertical_offset =
      let x,y,w,h =
        Sdl.Rect.x rect_vertical_offset,
        Sdl.Rect.y rect_vertical_offset,
        Sdl.Rect.w rect_vertical_offset,
        Sdl.Rect.h rect_vertical_offset
      in
      Sdl.Rect.create (x+offset) (y) w h
    in

    let center_rect_horizontal_offset =
      let x,y,w,h =
        Sdl.Rect.x rect_horizontal_offset,
        Sdl.Rect.y rect_horizontal_offset,
        Sdl.Rect.w rect_horizontal_offset,
        Sdl.Rect.h rect_horizontal_offset
      in
      Sdl.Rect.create (x) (y+offset) w h
    in



    (* Render the center *)
    let () = 
      for i = 0 to nb_horizontal_bar-1 do
        for j = 0 to nb_vertical_bar-1 do
          MTexture.render renderer ~clip_src:(Some rects.center) ~x:(interface.x+offset*i) ~y:(interface.y+offset*j) ~flip:Sdl.Flip.horizontal txt;
        done;
      done;


      (* Render the offset *)
      if vertical_offset > 0 then
        for i = 0 to nb_horizontal_bar-1 do
          begin
            MTexture.render renderer ~clip_src:(Some center_rect_vertical_offset) ~x:(interface.x+i*offset) ~y:(interface.y+offset*nb_vertical_bar) txt;
          end;
        done;

      if horizontal_offset > 0 then
        begin
          for i = 0 to nb_vertical_bar-1 do
            MTexture.render renderer ~clip_src:(Some center_rect_horizontal_offset) ~x:(interface.x+offset*nb_horizontal_bar) ~y:(interface.y+offset*i) txt;
          done;
        end;
    in

    let () = if vertical_offset > 0 && horizontal_offset > 0 then
        begin
          match Sdl.intersect_rect center_rect_vertical_offset center_rect_horizontal_offset with
          | None -> ()
          | Some r ->
            MTexture.render renderer ~clip_src:(Some r) ~x:(interface.x+offset*nb_horizontal_bar) ~y:(interface.y+offset*nb_vertical_bar) txt
        end
    in

    (* Resize *)
    match rects.resize with
    | None -> ()
    | Some _ -> MTexture.render renderer ~clip_src:(rects.resize) ~x:(interface.x+interface_w - offset) ~y:(interface.y+interface_h-offset) txt;


      (* The whole interface currently displayed,
         only the interface at the top of the list can
         be interacted with, the rest is only displayed *)
  type structure = t MTree.tree list

  let render_struct renderer interface_struct textures =

    List.iter (fun x ->
        let post_process_tree = 
          let tmp1 = set_to_relative_coordinate x in
          let tmp2 = set_to_relative_size tmp1 in
          tmp2
        in

        MTree.iter post_process_tree (fun y ->
            render renderer y.interface textures
          )
      ) interface_struct

  (* Miscellaneous functions *)
  let center parent_width child_width =
    let fwp,fwc =
      float parent_width,
      float child_width
    in
    round ((fwp -. fwc) /. 2.)




end
