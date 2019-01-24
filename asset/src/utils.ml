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

(* Check if key 'i' has been pressed *)
let check_key_scan ks i =
    (ks.{i} = 1)

(* Check event type *)
let check_ev_type e t =
    Sdl.Event.get e Sdl.Event.typ = t

let check_ev_key_repeat e =
    Sdl.Event.get e Sdl.Event.keyboard_repeat = 0

let get_scancode e =
    Sdl.Event.get e Sdl.Event.keyboard_scancode




