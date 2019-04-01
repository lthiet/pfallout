open Tsdl
open Texture_wrapper

module MBtn = struct

  let width = 400
  let height = 260

  type role =
    | START
    | SETTINGS
    | OPTION

  let idle = Sdl.Rect.create 0 0 width height
  let pressed = Sdl.Rect.create width 0 width height

  type status =
    | IDLE
    | PRESSED
    | RELEASED

  let status_to_clip s =
    match s with
    | IDLE -> idle
    | PRESSED -> pressed
    | _ -> idle

  type t = {
    x : int;
    y : int;
    role : role;
    status : status
  }

  let create x y role = {
    x = x;
    y = y;
    role = role;
    status = IDLE;
  }

  let is_pressed btn = 
    match btn.status with
    | PRESSED -> true
    | _ -> false

  let is_released btn = 
    match btn.status with
    | RELEASED -> true
    | _ -> false

  let get_role btn =
    btn.role


  let get_coord btn =
    btn.x,btn.y,width,height


  (* Render a btn *)
  let render renderer btn texture = 

    (* Render the button *)
    let clip = status_to_clip btn.status in
    MTexture.render renderer
      ~clip: (Some clip)
      ~x:btn.x
      ~y:btn.y
      texture

  let render_text renderer btn texture = 
    let offset_base = 10 in 
    let offset_pressed = if is_pressed btn then
        10
      else if is_released btn then
        -100
      else
        0
    in
    let final_offset = offset_base + offset_pressed in
    MTexture.render renderer
      ~x:(btn.x + final_offset)
      ~y:(btn.y + offset_base)
      texture
end
;;