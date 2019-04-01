open Tsdl
open Tsdl_image
open Tsdl_ttf

let load_image renderer path =
  match Image.load(path) with
  | Error (`Msg e) -> Error (`Msg e)
  | Ok surface ->
    match Sdl.create_texture_from_surface renderer surface with
    | Error (`Msg e) -> Error (`Msg e)
    | Ok res ->
      Sdl.free_surface surface; Ok res

let write_ttf renderer font font_size color s =
  match Ttf.open_font font font_size with
  | Error (`Msg e) -> Error (`Msg e)
  | Ok font ->
    match Ttf.render_text_solid font s color with
    | Error (`Msg e) -> Error (`Msg e)
    | Ok surface -> match Sdl.create_texture_from_surface renderer surface with
      | Error (`Msg e) -> Error (`Msg e)
      | Ok texture -> Sdl.free_surface surface; Ok texture

let draw_filled_rectangle renderer (r, g, b, a) (up, down, left, right) =
  match Sdl.set_render_draw_color renderer r g b a with
  | Error (`Msg e) -> Error (`Msg e)
  | Ok () ->
    match Sdl.set_render_draw_blend_mode renderer Sdl.Blend.mode_blend with
    | Error (`Msg e) -> Error (`Msg e)
    | Ok () ->
      match Sdl.enclose_points [
        Sdl.Point.create left up;
        Sdl.Point.create right up;
        Sdl.Point.create right down;
        Sdl.Point.create left down
      ] with
      | None -> Error (`Msg "Error creating rectangle")
      | Some rect -> Sdl.render_fill_rect renderer (Some rect)

let render_texture renderer (up, down, left, right) texture flip =
  match Sdl.enclose_points [
    Sdl.Point.create left up;
    Sdl.Point.create right up;
    Sdl.Point.create right down;
    Sdl.Point.create left down
  ] with
  | None -> Error (`Msg "Error creating rectangle")
  | Some rect -> Sdl.render_copy_ex renderer texture ~dst:rect 0.0 None flip
