module MAction_enum = struct
    type t = ATTACK | MOVE | PRODUCE | NOTHING
    | REFILL_MP

    let to_str t =
    match t with
    | ATTACK -> "ATTACK"
    | MOVE -> "MOVE"
    | PRODUCE -> "PRODUCE"
    | NOTHING -> "NOTHING"
    | REFILL_MP -> "REFILL_MP"

    let print t =
    Printf.printf "%s\n" (to_str t)
end
;;
