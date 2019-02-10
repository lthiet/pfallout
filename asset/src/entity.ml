open Game_object
open Action_enum

module MEntity = struct
    class entity r q hp ap mp current_mp atks defs pa aos =
    object(self)
        inherit game_object r q as super
        val hp : int = hp (* HEALTH POINT *)
        val ap : int = ap (* ARMOR POINT *)
        val mp : int = mp (* MOVEMENT POINT *)
        val current_mp : int = current_mp (* CURRENT MOVEMENT POINT *)
        val atks : int = atks (* STRENGTH ON ATTACK *)
        val defs : int = defs (* STRENGTH ON DEFENSE *)
        val pa : MAction_enum.t list = pa (* POSSIBLE ACTIONS *)
        val aos : MAction_enum.t list = aos (* ACTIONS ON START *)

        method get_hp = hp
        method get_ap = ap
        method get_mp = ap
        method can_use_mp n = 0 <= current_mp - n
        method use_mp n = 
            let x = max 0 (mp-n) in
            new entity r q hp ap mp x atks defs pa aos

        method get_atks = atks
        method get_defs = defs
        method get_pa = pa
        method get_aos = aos
    end

    type t = entity
end
;;