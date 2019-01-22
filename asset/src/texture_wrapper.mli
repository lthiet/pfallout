open Tsdl

module LTexture : sig
    type t
    val load_from_file : Sdl.renderer -> string -> t
    val free : t -> unit
    val render : Sdl.renderer 
        -> ?clip:Sdl.rect option
        -> ?x:int
        -> ?y:int
        -> ?angle:float
        -> ?center:Sdl.point option
        -> ?flip:Sdl.flip
        -> t
        -> unit
    val set_color : Sdl.uint8 -> Sdl.uint8 -> Sdl.uint8 -> t -> unit
    val set_blend_mode : t -> Sdl.Blend.mode -> unit
    val set_alpha : t -> Sdl.uint8 -> unit
end