(* Deals with special effects on the game *)

open Texture_pack
open Texture_wrapper
open Utils
open Tsdl
open Camera
module MFx = struct
  type code = HEALING | ATTACKED

  type t = {
    code : code
  }

  let fx_to_texture fx texture =
  match fx.code with
  | HEALING -> MTexture_pack.get_fx_healed texture
  | ATTACKED -> MTexture_pack.get_fx_attacked texture

  (* Render the fx *)
  let render renderer x y fx texture frame_n =
    let texture = fx_to_texture fx texture in
    let texture_width = (MTexture.get_w texture)/3 in
    let texture_height = (MTexture.get_h texture)/3 in
    let clip = Sdl.Rect.create ((frame_n/3)*texture_width) 0 texture_width texture_height in
    MTexture.render renderer ~clip:(Some clip) ~x:x ~y:y texture

end