open Tsdl

module LTexture : sig
    type lTexture
    val load_from_file : Sdl.renderer -> string -> lTexture
    val free : lTexture -> unit
    val render : Sdl.renderer -> lTexture -> int -> int -> unit
end