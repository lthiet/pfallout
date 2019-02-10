open Action_enum
open Entity
open Texture_wrapper
open Utils
open Hex
open Tsdl

module MMilitary = struct
    type military_type = SOLDIER | SNIPER
    type attack_type = MELEE | RANGED
    type terrain_type = GROUND | AIR

    class military r q hp ap mp current_mp atks defs ar pa aos mt at tt pc = 
    object(self)
        inherit MEntity.entity r q hp ap mp current_mp atks defs ar pa aos as super
        val mt : military_type = mt
        val at : attack_type = at
        val tt : terrain_type = tt
        val pc : int = pc (* Production cost *)
        method get_mt = mt
        method get_at = at
        method get_tt = tt
        method get_pc = pc
    end

    type t = military

    let create_soldier r q =
        new military r q 40 20 2 2 40 40 1 [MAction_enum.MOVE;MAction_enum.ATTACK] [] SOLDIER MELEE GROUND 3

    let military_to_entity m =
        (m : military :> MEntity.entity)

    let render renderer m military_texture camera =
        if check_collision m#get_box camera then
            let x,y = 
                let tmp1,tmp2 = MHex.axial_to_screen_coord m#get_axial in
                tmp1 - Sdl.Rect.x camera,tmp2 - Sdl.Rect.y camera
            in
            MTexture.render renderer
            ~x:x
            ~y:y
            military_texture



end
;;