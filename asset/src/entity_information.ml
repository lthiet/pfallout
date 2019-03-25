open Healthbar 
open Entity
open Entity_enum
open Faction_enum
open Texture_pack
open Texture_wrapper
open Tsdl
open Hex
(* This module will display the information of an entity, (like mp left, healthbar etc... ), on the map of a unit *)

module MEntity_information = struct
  type t = {
    x : int;
    y : int;
    healthbar : MHealthbar.t;
    mp_left : int;
    faction_enum  : MFaction_enum.t
  }

  let create x y healthbar mp_left faction_enum = 
    {
      x = x;
      y = y;
      healthbar = healthbar;
      mp_left = mp_left;
      faction_enum = faction_enum
    }

  (* It is required to specify the on screen coordinates of the entity *)
  let get_info entity x y = 
    let healthbar = MHealthbar.create (MEntity_enum.max_hp_of entity#get_ut) entity#get_hp in
    let mp_left = entity#get_current_mp in
    let faction_enum = entity#get_faction in
    create x y healthbar mp_left faction_enum 

  let render renderer textures info = 
    (* render the healthbar *)
    MHealthbar.render renderer info.x (info.y - 40) info.healthbar;

    (* render the mp left *)
    (* create the texture for the mp left *)
    let txt = MTexture_pack.get_mp_left textures info.mp_left in
    let txt_outline = MTexture_pack.get_mp_left_outline textures info.mp_left in
    let outline_size = MTexture_pack.get_mp_left_outline_size textures in
    let x = info.x + (MHex.width / 2) - ((MTexture.get_w txt_outline)/2 ) in
    let y = info.y + 120 in

    MTexture.render renderer ~x:x ~y:y txt_outline;
    MTexture.render renderer ~x:(outline_size+x) ~y:(outline_size+y) txt;




end


