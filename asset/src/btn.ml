open Tsdl
open Texture_wrapper

module MBtn = struct

    let width = 400
    let height = 260

    type role =
    | START
    | QUIT
    | OPTION

    let idle = Sdl.Rect.create 0 0 width height
    let pressed = Sdl.Rect.create width 0 width height

    type status =
    | IDLE
    | PRESSED

    let status_to_clip s =
        match s with
        | IDLE -> idle
        | PRESSED -> pressed

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
        MTexture.render renderer
        ~x:(btn.x + 10)
        ~y:(btn.y + 10)
        texture
end
;;