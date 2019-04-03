open Game_object
open Action_enum
open Utils
open Hex
open Tsdl
open Texture_wrapper
open Texture_pack
open Faction_enum
open Behaviour_enum
open Entity_enum
open Layer_enum
open Inventory

module MEntity = struct
  type unit_type = MEntity_enum.t 
  type layer_type = MLayer_enum.t
  type attack_type = MELEE | RANGED
  type terrain_type = GROUND | AIR
  type status = IDLE | ATTACKING | MOVING

  let status_to_string s =
    match s with
    | IDLE -> "idle"
    | ATTACKING -> "attacking"
    | MOVING -> "moving"


  let layer_to_string s =
    match s with
    | MLayer_enum.MILITARY -> "MILITARY"
    | MLayer_enum.INFRASTRUCTURE -> "INFRASTRUCTURE"

  let identifier = ref 0


  exception Unsifficient_mp
  class entity id r q hp ap mp current_mp atks defs ar pa aos faction ut lt at tt pc behaviour = 
    object(self)
      inherit game_object r q as super

      val truc : unit = ()
      (* Identification *)
      val id : int = id
      method get_id = id

      (* Unit type *)
      val ut : unit_type = ut

      (* Health points *)
      val hp : int = hp 
      method get_hp = hp
      method add_hp_max amount = {< hp = max (MEntity_enum.max_hp_of ut) (self#get_hp + amount)>}
      method is_low_hp = 
        let threshold = (MEntity_enum.max_hp_of ut)/2 in
        hp <= threshold

      (* Invetory *)
      val inventory : MInventory.t = MInventory.empty
      method get_inventory = inventory
      method set_inventory x = {< inventory = x >}
      method add_item_to_inventory item = {< inventory = MInventory.add_item inventory item>}

      val ap : int = ap (* ARMOR POINT *)
      val mp : int = mp (* MOVEMENT POINT *)
      val current_mp : int = current_mp (* CURRENT MOVEMENT POINT *)
      val atks : int = atks (* STRENGTH ON ATTACK *)
      val defs : int = defs (* STRENGTH ON DEFENSE *)
      val ar : int = ar (* ATTACK RANGE *)
      val pa : MAction_enum.enum list = pa (* POSSIBLE ACTIONS *)
      val aos : MAction_enum.enum list = aos (* ACTIONS ON START *)
      val faction : MFaction_enum.t = faction 
      val lt : layer_type = lt
      val at : attack_type = at
      val tt : terrain_type = tt
      val pc : int = pc (* Production cost *)
      val status : status = IDLE
      val behaviour : MBehaviour_enum.t = behaviour
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
      method get_lt = lt
      method get_at = at
      method get_tt = tt
      method get_pc = pc
      method set_status s = {<status = s>}
      method get_status = status
      method set_behaviour b = {<behaviour = b>}
      method get_behaviour = behaviour
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
      method empty_mp = {<current_mp = 0>}
      method check_layer l = self#get_lt = l
      method check_unit_type ut = self#get_ut = ut
    end
  type t = entity

  let create r q hp ap mp current_mp atks defs ar pa aos faction ut lt at tt pc = 
    let new_id = incr identifier in
    new entity new_id r q hp ap mp current_mp atks defs ar pa aos faction ut lt at tt pc

  let create_fx_binder () =
    new entity (-1) 0 0 0 0 0 0 0 0 0 [] [] (MFaction_enum.EU,-1) MEntity_enum.FX_BINDER MLayer_enum.MILITARY MELEE GROUND 0 MBehaviour_enum.WANDERING

  let is_infrastructure t =
    match t#get_ut with
    | MEntity_enum.CITY -> true
    | _-> false

  let is_military t =
    not (is_infrastructure t)

  let to_string t =
    (MHex.to_string_ax t#get_axial) 
    ^ " hp : " ^ (string_of_int t#get_hp)
    ^ " current mp : " ^(string_of_int t#get_current_mp)
    ^ " layer : " ^ (layer_to_string t#get_lt)
    ^ " behaviour : " ^(MBehaviour_enum.to_string t#get_behaviour)
    ^ " inventory : " ^(MInventory.to_string t#get_inventory)

  let entity_textures entity texture =
    (* the faction of the entity *)
    let f = entity#get_faction
    in
    if is_military entity then
      begin
        match f with
        | MFaction_enum.EU,_ ->
          MTexture_pack.get_soldier_eu texture
        | MFaction_enum.ASIA,_ ->
          MTexture_pack.get_soldier_pac texture
        | MFaction_enum.USA,_ ->
          MTexture_pack.get_soldier_us texture
        | _ -> raise Not_yet_implemented
      end
    else
      begin
        MTexture_pack.get_city texture
      end



  let get_clip frame_n e =
    (* TODO : textures wip *)
    match e#get_faction with
    | MFaction_enum.USA,_ -> Sdl.Rect.create 0 0 MHex.width MHex.height
    | _ -> 
      match e#get_status with
      | MOVING ->
        Sdl.Rect.create (MHex.width * (frame_n/7)) 0 MHex.width MHex.height
      | ATTACKING ->
        Sdl.Rect.create (MHex.width * (frame_n/7)) MHex.height MHex.width MHex.height
      | _ ->
        Sdl.Rect.create 0 0 MHex.width MHex.height 

  exception Option_coord_need_to_be_both_none_or_some

  (* Render the entity and return the coordinate at which it was rendered*)
  let render renderer 
      ?(x:int option = None) 
      ?(y:int option = None)
      e texture camera frame_n =
    match e#get_ut with
    | MEntity_enum.FX_BINDER -> 0,0
    | _ ->
      let clip = 
        if is_military e then
          let tmp = get_clip frame_n e in 
          Some tmp
        else
          None
      in
      let pos_x,pos_y = 
        match x,y with
        | None,None ->
          let tmp1,tmp2 = MHex.axial_to_screen_coord e#get_axial in
          tmp1 - Sdl.Rect.x camera,tmp2 - Sdl.Rect.y camera
        | (Some x),(Some y) ->
          x - Sdl.Rect.x camera,y - Sdl.Rect.y camera
        | _,_ -> raise Option_coord_need_to_be_both_none_or_some
      in

      let () =
        if check_collision e#get_box camera then
          let txt = entity_textures e texture in

          (* Then render the entity *)
          MTexture.render renderer
            ~clip:(clip)
            ~x:pos_x
            ~y:pos_y
            txt;
      in pos_x,pos_y
end
;;