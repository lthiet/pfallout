module MBehaviour_enum = struct
  type t =
    | WANDERING
    (*  The attacking enum needs the identifier of the target entity,
    and the range at which the source entity will look for the target entity*)
    | ATTACKING of int * int 
    | DEFENDING
    | SPAWNING
    | CONTROLLED_BY_PLAYER

  let to_string t =   
    match t with
    | WANDERING -> "WANDERING"
    | ATTACKING _ -> "ATTACKING"
    | DEFENDING -> "DEFENDING"
    | SPAWNING -> "SPAWNING"
    | CONTROLLED_BY_PLAYER -> "CONTROLLED_BY_PLAYER"

end