open Tsdl
open Utils
open Keyboard_wrapper

module MCamera = struct
  let vel_max = 35

  type t = {
    rect : Sdl.rect;
    vel_x : int;
    vel_y : int
  }

  let create r =
    {
      rect = r;
      vel_x = 0;
      vel_y = 0
    }

  let get_rect t = t.rect
  let get_vel_x t = t.vel_x
  let get_vel_y t = t.vel_y

  (* Update the position of the camera according to its velocity,
     and update its size according to the window *)
  let update_camera window c =
    let r = c.rect in
    let x,y=
      Sdl.Rect.x r,
      Sdl.Rect.y r
    in
    let w,h = Sdl.get_window_size window in

    let new_rect = Sdl.Rect.create (x + c.vel_x) (y + c.vel_y) w h
    in
    {
      c with
      rect = new_rect
    }


  (* Based on input/event, change the camera direct *)
  let change_direction c e =
    (* When key is pressed, add velocity *)
    if check_ev_type e Sdl.Event.key_down && MKeyboard.check_ev_key_repeat e then
      let pk = MKeyboard.get_scancode e in
      let new_vel_x =
        if pk = Sdl.Scancode.d then
          c.vel_x + vel_max
        else if pk = Sdl.Scancode.a then
          c.vel_x - vel_max
        else
          c.vel_x
      in
      let new_vel_y =
        if pk = Sdl.Scancode.s then
          c.vel_y + vel_max
        else if pk = Sdl.Scancode.w then
          c.vel_y - vel_max
        else
          c.vel_y
      in
      {
        c with
        vel_x = new_vel_x;
        vel_y = new_vel_y
      }
      (* When key is released, remove velocity *)
    else if check_ev_type e Sdl.Event.key_up && MKeyboard.check_ev_key_repeat e then
      let pk = MKeyboard.get_scancode e in
      let new_vel_x =
        if pk = Sdl.Scancode.d then
          c.vel_x - vel_max
        else if pk = Sdl.Scancode.a then
          c.vel_x + vel_max
        else
          c.vel_x
      in
      let new_vel_y =
        if pk = Sdl.Scancode.s then
          c.vel_y - vel_max
        else if pk = Sdl.Scancode.w then
          c.vel_y + vel_max
        else
          c.vel_y
      in
      {
        c with
        vel_x = new_vel_x;
        vel_y = new_vel_y
      }

    (* Otherwise nothing to do *)
    else
      c

  let scale t r= 
    let rect = t.rect in
    let x,y,w,h =
      Sdl.Rect.x rect,
      Sdl.Rect.y rect,
      Sdl.Rect.w rect,
      Sdl.Rect.h rect
    in
    let x',y',w',h' =
      scale_to x r,
      scale_to y r,
      scale_to w (inverse r),
      scale_to h (inverse r)
    in
    {
      t with
      rect =  Sdl.Rect.create x' y' w' h'
    }


end
