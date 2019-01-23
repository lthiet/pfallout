open Tsdl
open Utils
open Texture_wrapper

module LButton : sig
    type t

    val default : t 
    val button_width : int 
    val button_height : int 
    val set_pos : t -> int -> int -> t
    val handle_event : t -> Sdl.Event -> t
    val render : Sdl.renderer -> t -> LTexture.t -> Sdl.Rect array -> unit
end