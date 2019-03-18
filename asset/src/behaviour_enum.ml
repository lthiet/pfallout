module MBehaviour_enum = struct
  type t =
    | WANDERING
    | ATTACKING
    | DEFENDING
    | SPAWNING
    | CONTROLLED_BY_PLAYER

  let to_string t =   
    match t with
    | WANDERING -> "WANDERING"
    | ATTACKING -> "ATTACKING"
    | DEFENDING -> "DEFENDING"
    | SPAWNING -> "SPAWNING"
    | CONTROLLED_BY_PLAYER -> "CONTROLLED_BY_PLAYER"

end