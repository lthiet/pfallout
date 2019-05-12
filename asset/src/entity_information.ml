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
    if info.displayed then
      begin
        (* render the healthbar *)
        MHealthbar.render renderer scale info.x (info.y - 40) info.healthbar;

        (* create the texture for the mp left *)
        let outline_size = 8 in
        let font_mp_left,font_mp_left_outline = MFont_pack.open_font_with_outline MFont_pack.good_times 65 outline_size in
        let txt = MTexture.load_from_rendered_text renderer font_mp_left (string_of_int info.mp_left) MColor.white in
        let txt_outline = MTexture.load_from_rendered_text renderer font_mp_left_outline (string_of_int info.mp_left) MColor.black in
        let x_mp = info.x + (MHex.width / 2) - ((MTexture.get_w txt_outline)/2 ) in
        let y_mp = info.y + (round ((float_of_int MHex.height) *. 0.85)) in
        (* render the mp left *)
        MTexture.render renderer ~x:x_mp ~y:y_mp ~scale:scale txt_outline;
        MTexture.render renderer ~x:(outline_size+x_mp) ~y:(outline_size+y_mp) ~scale:scale txt;

        (* Free *)
        MTexture.free txt;
        MTexture.free txt_outline;
        Ttf.close_font font_mp_left;
        Ttf.close_font font_mp_left_outline;

        (* render the id *)
        (* let font_id = MFont_pack.open_font MFont_pack.good_times 50 in
        let txt_id = MTexture.load_from_rendered_text renderer font_id (string_of_int info.id) MColor.green in
        let x_id = info.x in
        let y_id = info.y in *)

        (* Free *)
        (* MTexture.render renderer ~x:x_id ~y:y_id ~scale:scale txt_id;
        MTexture.free txt_id;
        Ttf.close_font font_id; *)
      end
end


