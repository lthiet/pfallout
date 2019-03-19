open Tsdl
open Texture_wrapper

module MWdw = struct

    (*Champs de titre*)

    (* a window is defined by its starting postition, its witdh and its height*)
    type t = {
        x : int;
        y : int;
        w : int;
        h : int
    }

    type status = 
        | OPENED
        | CLOSED

    (* returns the positions, width and height *)
    let get_coord wdw =
        wdw.x,wdw.y,wdw.w,wdw.h

    let render renderer wdw texture = 
        MTexture.render renderer
        ~x:wdw.x
        ~y:wdw.y
        texture


