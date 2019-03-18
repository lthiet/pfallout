open Game_object
open Texture_pack
open Utils
open Hex
open Texture_wrapper
open Tsdl

module MItem = struct
  (* The kind of the object *)
  type code = HEALTHPACK of int | NUKE

  class item r q code = 
    object(self)
      inherit game_object r q as super
      val code : code = code 

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