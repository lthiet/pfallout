open Game_object
open Action_enum
open Utils
open Hex
open Tsdl
open Texture_wrapper
open Texture_pack
open Faction_enum

module MEntity = struct
  exception Unsifficient_mp
  type unit_type = SOLDIER | SNIPER | CITY
  type attack_type = MELEE | RANGED
  type terrain_type = GROUND | AIR

  type status = IDLE | ATTACKING | MOVING

  let status_to_string s =
    match s with
    | IDLE -> "idle"
    | ATTACKING -> "attacking"
    | MOVING -> "moving"

  class entity r q hp ap mp current_mp atks defs ar pa aos faction ut at tt pc = 
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
      val ut : unit_type = ut
      val at : attack_type = at
      val tt : terrain_type = tt
      val pc : int = pc (* Production cost *)
      val status : status = IDLE
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
      method get_ut = ut
      method get_at = at
      method get_tt = tt
      method get_pc = pc
      method set_status s = {<status = s>}
      method get_status = status
      method check_status s = self#get_status = s
      method remove_mp n = 
        let tmp = self#get_current_mp-n in
        if tmp >= 0 then
          {< current_mp = tmp>}
        else
          raise Unsifficient_mp
      method remove_hp damage = {< hp = self#get_hp-damage>}
      method refill_mp = {<current_mp = self#get_mp>}
      method can_move = self#get_current_mp <> 0

    end
  type t = entity

  let create r q hp ap mp current_mp atks defs ar pa aos faction ut at tt pc = 
    new entity r q hp ap mp current_mp atks defs ar pa aos faction ut at tt pc

  let is_infrastructure t =
    match t#get_ut with
    | CITY -> true
    | _-> false

  let is_military t =
    not (is_infrastructure t)

  let to_string t =
    (MHex.to_string_ax t#get_axial) ^ " current mp : " ^(string_of_int t#get_current_mp)

  let entity_textures entity texture =
    (* the faction of the entity *)
    let f = entity#get_faction
    in

    match f with
    | MFaction_enum.EU,_ ->
      MTexture_pack.get_soldier_eu texture
    | MFaction_enum.ASIA,_ ->
      MTexture_pack.get_soldier_pac texture
    | _ -> raise Not_yet_implemented

  let get_clip frame_n e =
    match e#get_status with
    | MOVING ->
      Sdl.Rect.create (MHex.width * (frame_n/3)) 0 MHex.width MHex.height
    | _ ->
      Sdl.Rect.create 0 0 MHex.width MHex.height 


  (* Render the entity *)
  let render renderer e texture camera frame_n =
    let clip = get_clip frame_n e in
    if check_collision e#get_box camera then
      let x,y = 
        let tmp1,tmp2 = MHex.axial_to_screen_coord e#get_axial in
        tmp1 - Sdl.Rect.x camera,tmp2 - Sdl.Rect.y camera
      in
      let txt = entity_textures e texture in
      MTexture.render renderer
        ~clip:(Some clip)
        ~x:x
        ~y:y
        txt

end
;;