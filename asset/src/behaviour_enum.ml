module MBehaviour_enum = struct
  type t =
    | WANDERING
    | ATTACKING
    | DEFENDING

  let to_string t =   
    match t with
    | WANDERING -> "WANDERING"
    | ATTACKING -> "ATTACKING"
    | DEFENDING -> "DEFENDING"

end