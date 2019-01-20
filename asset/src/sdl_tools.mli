open Tsdl
open Tsdl_ttf

(** [load_image renderer path] returns a texture of the image found in [path]
  * to be rendered on [renderer]. *)
val load_image: Sdl.renderer -> string -> Sdl.texture Sdl.result
(** [write_ttf renderer font_name font_size color contents] returns a texture
  * representing the text [contents] rendered in [renderer] with the font
  * [font_name] in size [font_size] and color [color]. *)
val write_ttf: Sdl.renderer -> string -> int -> Sdl.color -> string -> Sdl.texture Sdl.result
(** [draw_filled_rectangle renderer (r, g, b, a) (up, down, left, right)] draws
  * a filled rectangle with the color [r, g, b, a] in position [up, down, left,
  * right]. *)
val draw_filled_rectangle: Sdl.renderer -> (int * int * int * int) -> (int * int * int * int) -> unit Sdl.result
(** [render_texture renderer (up, down, left, right) texture flip] renders
  * [texture] in [renderer] in position [up, down, left, right] flipped
  * according to [flip]. *)
val render_texture: Sdl.renderer -> (int * int * int * int) -> Sdl.texture -> Sdl.flip -> unit Sdl.result
