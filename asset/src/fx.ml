(* Deals with special effects on the game *)

open Texture_pack
open Texture_wrapper
open Utils
open Tsdl
open Camera
open Hex
module MFx = struct
  type code = HEALING | ATTACKED

  type t = {
    code : code
  }

  let create code = { code = code }

  let fx_to_texture fx texture =
    match fx.code with
    | HEALING -> MTexture_pack.get_fx_healed texture
    | ATTACKED -> MTexture_pack.get_fx_attacked texture

  let get_clip frame_n =
    Sdl.Rect.create (MHex.width * (frame_n/7)) 0 MHex.width MHex.height



  (* Render the fx *)
  let render renderer x y fx texture camera frame_n =
    let texture = fx_to_texture fx texture in

    (* let clip = Sdl.Rect.create ((frame_n/3)*texture_width) 0 texture_width texture_height in *)
    let clip =  get_clip frame_n in
    MTexture.render renderer ~clip:(Some clip) ~x:(x - Sdl.Rect.x camera) ~y:(y - Sdl.Rect.y camera) texture

end