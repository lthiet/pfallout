open Tsdl

(* Utils function *)
(* Manage Result *)
let manage_result r s =
    match r with
    | Ok a -> a
    | Error (`Msg e) -> Sdl.log s e;exit 1
    
(* Manage Option *)
let manage_option r s =
    match r with
    | Some x -> x
    | None -> Sdl.log s;exit 1

(* Get format field from a surface *)
let get_format_from_surface surface =
    let enum = Sdl.get_surface_format_enum surface in
    manage_result (
        Sdl.alloc_format enum
    ) "Error alloc format %s"

(* Make a rectangle *)
let make_rect x y w h =
    Sdl.Rect.create x y w h 

