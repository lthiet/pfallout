open Healthbar 
open Entity
open Entity_enum
open Faction_enum
open Texture_pack
open Texture_wrapper
open Tsdl
open Tsdl_ttf
open Hex
open Colors
open Font_pack
open Utils
open Lru_cache
(* This module will display the information of an entity, (like mp left, healthbar etc... ), on the map of a unit *)

module MEntity_information = struct
  type basic = {
    id : int;
    x : int;
    y : int;
    displayed : bool;
    healthbar : MHealthbar.t;
    mp_left : int;
    faction_enum  : MFaction_enum.t;
  }

  let create id x y displayed healthbar mp_left faction_enum = 
    {
      id = id;
      x = x;
      y = y;
      displayed = displayed;
      healthbar = healthbar;
      mp_left = mp_left;
      faction_enum = faction_enum
    }

  (* It is required to specify the on screen coordinates of the entity *)
  let get_info entity x y displayed = 
    match entity#get_ut with
    | MEntity_enum.FX_BINDER -> None
    | _ ->
      let healthbar = MHealthbar.create (MEntity_enum.max_hp_of entity#get_ut) entity#get_hp in
      let mp_left = entity#get_mp in
      let faction_enum = entity#get_faction in
      let id = entity#get_id in
      let tmp = create id x y displayed healthbar mp_left faction_enum in
      Some tmp

  let render renderer textures scale info = 
    let () = debug (string_of_int (MLRUCache.Lru_cache.cardinal textures)) in
    if info.displayed then
      begin
        (* render the healthbar *)
        MHealthbar.render renderer scale info.x (info.y - 40) info.healthbar;

        (* Create texture for mp left and render it *)
        (* Fetch the texture *)
        let outline_size = 8 in
        let tpack,txt_text,txt_outline =
          let f_txt_text = MTexture_pack.texture_creation_from_text renderer 0 MFont_pack.good_times 80 MColor.white (String.concat "" [string_of_int info.mp_left;" MP"]) in
          let f_txt_outline = MTexture_pack.texture_creation_from_text renderer outline_size MFont_pack.good_times 80 MColor.black (String.concat "" [string_of_int info.mp_left;" MP"]) in
          let txt_text,tpack1 = MTexture_pack.fetch_texture (String.concat "" [string_of_int info.mp_left;"_MP_TEXT"]) textures f_txt_text MTexture.free in
          let txt_outline,tpack2 = MTexture_pack.fetch_texture (String.concat "" [string_of_int info.mp_left;"_MP_OUTLINE"]) tpack1 f_txt_outline MTexture.free in
          tpack2,txt_text,txt_outline
        in
        let x_mp = info.x + (MHex.width / 2) - ((MTexture.get_w txt_outline)/2 ) in
        let y_mp = info.y + (round ((float_of_int MHex.height) *. 0.85)) in
        (* render the mp left *)
        MTexture.render renderer ~x:x_mp ~y:y_mp ~scale:scale txt_outline;
        MTexture.render renderer ~x:(outline_size+x_mp) ~y:(outline_size+y_mp) ~scale:scale txt_text;

        let () = debug (string_of_int (MLRUCache.Lru_cache.cardinal tpack)) in
        (* return the texture pack *)
        tpack
      end
    else
      textures
end


