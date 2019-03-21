(* Deals with special effects on the game *)

module MFx = struct
    type code = HEALING | ATTACKED

    type t = {
        code : code
    }
end