open Hex
open Item

module MBehaviour_enum = struct
  type t =
    | WANDERING
    (*  The attacking enum needs the identifier of the target entity,
        and the range at which the source entity will look for the target entity*)
    | ATTACKING of int * int 
    (* The going to healthpack needs to know where the healthpack item *)
    | GOING_TO_HEALTHPACK of MHex.axial_coord
    (* Which healthpack is used *)
    | USING_HEALTHPACK of MItem.t 
    | DEFENDING
    | SPAWNING
    | CONTROLLED_BY_PLAYER
    | FLEEING

  let to_string t =   
    match t with
    | WANDERING -> "WANDERING"
    | ATTACKING _ -> "ATTACKING"
    | USING_HEALTHPACK _  -> "USING_HEALTHPACK"
    | GOING_TO_HEALTHPACK _ -> "GOING TO HEALTHPACK"
    | DEFENDING -> "DEFENDING"
    | SPAWNING -> "SPAWNING"
    | CONTROLLED_BY_PLAYER -> "CONTROLLED_BY_PLAYER"
    | FLEEING -> "FLEEING"

end