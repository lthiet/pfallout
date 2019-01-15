(* Modules *)
open Tsdl.Sdl

(* Constants *)
val screen_width : int
val screen_height : int

(* Functions *)
val init : unit -> unit
val create_window : string -> int -> int -> window
val get_window_surface : window -> surface
