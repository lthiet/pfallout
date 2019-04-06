open Utils

(* Determines the type of the entity *)
module MEntity_enum = struct
  (* The FX binder unit type isn't displayed, isn't used anywhere else than animation *)
  type t = SOLDIER | SNIPER | CITY | FORGERY | FX_BINDER

  let max_hp_of_soldier = 50
  let max_hp_of_city = 100

  let max_hp_of t =
    match t with
    | SOLDIER -> max_hp_of_soldier
    | CITY -> max_hp_of_city
    | _ -> raise Not_yet_implemented
end
