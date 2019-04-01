(* Deals with special effects on the game *)

open Texture_pack
open Texture_wrapper
open Utils
open Tsdl
open Camera
open Hex
open Entity
module MFx = struct
  type code = HEALING | ATTACKED

  type t = {
    code : code;
    x : int;
    y : int
  }

  let create code x y = { 
    code = code;
    x = x;
    y = y;
  }

  let create_from_entity entity code =
    let x,y = MHex.axial_to_screen_coord entity#get_axial in
    {
      code = code;
      x = x;
      y = y;
    }

  let fx_to_texture fx texture =
    match fx.code with
    | HEALING -> MTexture_pack.get_fx_healed texture
    | ATTACKED -> MTexture_pack.get_fx_attacked texture

  let get_clip frame_n =
    Sdl.Rect.create (MHex.width * (frame_n/7)) 0 MHex.width MHex.height



  (* Render the fx *)
  let render renderer fx texture camera frame_n =
    let texture = fx_to_texture fx texture in

    (* let clip = Sdl.Rect.create ((frame_n/3)*texture_width) 0 texture_width texture_height in *)
    let clip =  get_clip frame_n in
    MTexture.render renderer ~clip:(Some clip) ~x:(fx.x - Sdl.Rect.x camera) ~y:(fx.y - Sdl.Rect.y camera) texture

end