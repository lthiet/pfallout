open Game_object
open Texture_pack
open Utils
open Hex
open Texture_wrapper
open Tsdl
open Layer_enum

module MItem = struct
  (* The kind of the object *)
  type code =
    (* Amount of hp healed *)
    | HEALTHPACK of int
    (* Radius and damage *)
    | NUKE of int * int

  (* The enum is the same as above but without any attributes to it,
     useful when the attributes aren't used*)
  type enum =
    | HEALTHPACK_E
    | NUKE_E
  type param =
    (* Health pack need a source to know which unit is healed, and a destination to know which item must be consumed *)
    | HEALTHPACK_P of MHex.axial_coord * MHex.axial_coord * MLayer_enum.t
    (* Nukes need a source and a destination*)
    | NUKE_P of MHex.axial_coord * MHex.axial_coord

  let create_healthpack_param src dst layer = 
    HEALTHPACK_P(src,dst,layer)

  let same_code_and_param code param = 
    match code,param with
    | HEALTHPACK _, HEALTHPACK_P _ -> true
    | NUKE _, NUKE_P _ -> true
    | _ -> false

  let same_code_and_enum code enum = 
    match code,enum with
    | HEALTHPACK _, HEALTHPACK_E -> true
    | NUKE _, NUKE_E -> true
    | _ -> false

  class item r q code = 
    object(self)
      inherit game_object r q as super
      val code : code = code 
      method get_code = code

      (* Determines whether or not the item is owned, if it is not owned, then it will be displayed *)
      val owned : bool = false
      method is_owned = owned
    end
  type t = item

  let create_healthpack r q heal_amount =
    new item r q (HEALTHPACK heal_amount)

  (* Render the item *)
  let render renderer item texture camera frame_n =
    if not item#is_owned then
      if check_collision item#get_box camera then
        let x,y = 
          let tmp1,tmp2 = MHex.axial_to_screen_coord item#get_axial in
          tmp1 - Sdl.Rect.x camera,tmp2 - Sdl.Rect.y camera
        in
        let txt = MTexture_pack.get_healthpack texture in
        MTexture.render renderer
          ~x:x
          ~y:y
          txt
end
;;