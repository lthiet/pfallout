module MBehaviour_enum :
  sig
    type t =
        WANDERING
      | ATTACKING of int * int
      | GOING_TO_ITEM of Hex.MHex.axial_coord * Item.MItem.enum
      | USING_ITEM of Item.MItem.t
      | DEFENDING
      | SPAWNING
      | CONTROLLED_BY_PLAYER
      | FLEEING
    val to_string : t -> string
  end
