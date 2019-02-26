open Game_object
open Action_enum
open Utils
open Hex
open Tsdl
open Texture_wrapper
open Faction_enum

module MEntity = struct
  exception Unsifficient_mp
  class entity r q hp ap mp current_mp atks defs ar pa aos faction =
    object(self)
      inherit game_object r q as super
      val hp : int = hp (* HEALTH POINT *)
      val ap : int = ap (* ARMOR POINT *)
      val mp : int = mp (* MOVEMENT POINT *)
      val current_mp : int = current_mp (* CURRENT MOVEMENT POINT *)
      val atks : int = atks (* STRENGTH ON ATTACK *)
      val defs : int = defs (* STRENGTH ON DEFENSE *)
      val ar : int = ar (* ATTACK RANGE *)
      val pa : MAction_enum.t list = pa (* POSSIBLE ACTIONS *)
      val aos : MAction_enum.t list = aos (* ACTIONS ON START *)
      val faction : MFaction_enum.t = faction 

      method get_hp = hp
      method get_ap = ap
      method get_mp = mp
      method get_current_mp = current_mp
      method get_atks = atks
      method get_defs = defs
      method get_pa = pa
      method get_ar = ar
      method get_aos = aos
      method get_faction = faction
      method remove_mp n = 
        let tmp = self#get_current_mp-n in
        if tmp >= 0 then
          {< current_mp = tmp>}
        else
          raise Unsifficient_mp
      method remove_hp damage = {< hp = self#get_hp-damage>}
      method refill_mp = {<current_mp = self#get_mp>}

    end
  type t = entity

  (* Render the entity *)
  let render renderer e txt camera frame_n=
    let clip = Sdl.Rect.create (MHex.width * frame_n) 0 MHex.width MHex.height in
    if check_collision e#get_box camera then
      let x,y = 
        let tmp1,tmp2 = MHex.axial_to_screen_coord e#get_axial in
        tmp1 - Sdl.Rect.x camera,tmp2 - Sdl.Rect.y camera
      in
      MTexture.render renderer
        ~clip:(Some clip)
        ~x:x
        ~y:y
        txt

end
;;