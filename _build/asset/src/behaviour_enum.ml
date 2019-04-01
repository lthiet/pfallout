open Hex
open Item

module MBehaviour_enum = struct
  type t =
    | WANDERING
    (*  The attacking enum needs the identifier of the target entity,
        and the range at which the source entity will look for the target entity*)
    | ATTACKING of int * int 
    (* The going to healthpack needs to know where the healthpack item *)
    | GOING_TO_ITEM of MHex.axial_coord * MItem.enum
    (* Which item is used *)
    | USING_ITEM of MItem.t 
    | DEFENDING
    | SPAWNING
    | CONTROLLED_BY_PLAYER
    | FLEEING

  let to_string t =   
    match t with
    | WANDERING -> "WANDERING"
    | ATTACKING _ -> "ATTACKING"
    | USING_ITEM item  -> "USING ITEM" ^ (MItem.to_string item)
    | GOING_TO_ITEM (_,item) -> "GOING TO ITEM" ^ (MItem.to_string_enum item)
    | DEFENDING -> "DEFENDING"
    | SPAWNING -> "SPAWNING"
    | CONTROLLED_BY_PLAYER -> "CONTROLLED_BY_PLAYER"
    | FLEEING -> "FLEEING"

end