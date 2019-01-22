open Tsdl

module LTexture : sig
    type t
    val load_from_file : Sdl.renderer -> string -> t
    val free : t -> unit
    val render : Sdl.renderer -> Sdl.rect option -> t -> int -> int -> unit
    val set_color : Sdl.uint8 -> Sdl.uint8 -> Sdl.uint8 -> t -> unit
end