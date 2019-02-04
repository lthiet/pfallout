open Tsdl
open Texture_wrapper
open Utils

(* Constants *)
module MBackground = struct
    
    let render renderer texture camera = 
        let t = texture in
        let t_w = MTexture.get_w t in
        let t_h = MTexture.get_h t in
        let c_x = (Sdl.Rect.x camera) in
        let c_y =  (Sdl.Rect.y camera) in
        let c_w =  (Sdl.Rect.w camera) in
        let c_h =  (Sdl.Rect.h camera) in

        for i = (c_x/t_w)-1 to (c_x/t_w)+(c_w/t_w)+1 do
            for j = (c_y/t_h)-1 to (c_y/t_h)+(c_h/t_h)+1 do
                MTexture.render renderer
                ~x:(i*t_w - c_x)
                ~y:(j*t_h - c_y)
                texture
            done;
        done;
end
;;