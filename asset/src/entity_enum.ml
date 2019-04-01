open Utils

(* Determines the type of the entity *)
module MEntity_enum = struct
    type t = SOLDIER | SNIPER | CITY | FORGERY 

    let max_hp_of_soldier = 30
    let max_hp_of_city = 100

    let max_hp_of t =
        match t with
        | SOLDIER -> max_hp_of_soldier
        | CITY -> max_hp_of_city
        | _ -> raise Not_yet_implemented
end
