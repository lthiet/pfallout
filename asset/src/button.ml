open Tsdl
open Texture_wrapper

module MButton = struct

  let width = 400
  let height = 260

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
    status : status
  }

  let create x y = {
    x = x;
    y = y;
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

  let get_coord btn =
    btn.x,btn.y,width,height


  (* Render a btn *)
  let render renderer scale btn texture = 

    (* Render the button *)
    let clip = status_to_clip btn.status in
    MTexture.render renderer
      ~clip_src: (Some clip)
      ~scale:scale
      ~x:btn.x
      ~y:btn.y
      texture

end
;;