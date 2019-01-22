open Tsdl

module LTexture : sig
    type lTexture
    val load_from_file : Sdl.renderer -> string -> lTexture
    val free : lTexture -> unit
    val render : Sdl.renderer -> Sdl.rect option -> lTexture -> int -> int -> unit
end