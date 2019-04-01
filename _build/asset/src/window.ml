open Tsdl
open Texture_wrapper

module MWindow = struct

    (*Champs de titre*)

    (* a window is defined by its starting postition, its witdh and its height*)

    type role = 
        |START
        |SETTINGS

    type t = {
        x : int;
        y : int;
        w : int;
        h : int;
        role : role
    }

    type status = 
        | OPENED
        | CLOSED

    (* creates a window *)
    let create x y w h role = {
        x = x;
        y = y;
        w = w;
        h = h;
        role = role;
    }

    let get_role t = 
        t.role

    (* returns the positions, width and height *)
    let get_coord wdw =
        wdw.x,wdw.y,wdw.w,wdw.h

    let render renderer wdw texture = 
        MTexture.render renderer
        ~x:wdw.x
        ~y:wdw.y
        texture
end

