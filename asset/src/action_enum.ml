module MAction_enum = struct
    type t = ATTACK | MOVE | PRODUCE | NOTHING

    let to_str t =
    match t with
    | ATTACK -> "ATTACK"
    | MOVE -> "MOVE"
    | PRODUCE -> "PRODUCE"
    | NOTHING -> "NOTHING"

    let print t =
    Printf.printf "%s\n" (to_str t)
end
;;
