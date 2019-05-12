module MAction_enum :
  sig
    type enum =
        ATTACK_E
      | MOVE_E
      | REFILL_MP_E
      | PASS_E
      | CHANGE_BEHAVIOUR_E
      | SPAWN_ENTITY_E
      | USE_ITEM_E of Item.MItem.enum
      | PICKUP_ITEM_E
    type t =
        ATTACK of Hex.MHex.axial_coord * Hex.MHex.axial_coord *
          Layer_enum.MLayer_enum.t * Layer_enum.MLayer_enum.t
      | MOVE of Hex.MHex.axial_coord * Hex.MHex.axial_coord *
          Layer_enum.MLayer_enum.t
      | REFILL_MP of Hex.MHex.axial_coord * Layer_enum.MLayer_enum.t
      | PASS of Hex.MHex.axial_coord * Layer_enum.MLayer_enum.t
      | CHANGE_BEHAVIOUR of Hex.MHex.axial_coord * Layer_enum.MLayer_enum.t
      | SPAWN_ENTITY of Hex.MHex.axial_coord * Hex.MHex.axial_coord *
          Entity_enum.MEntity_enum.t
      | USE_ITEM of Item.MItem.code * Item.MItem.param
      | PICKUP_ITEM of Hex.MHex.axial_coord * Hex.MHex.axial_coord *
          Layer_enum.MLayer_enum.t
    val action_on_start :
      enum ->
      < get_axial : Hex.MHex.axial_coord; get_lt : Layer_enum.MLayer_enum.t;
        .. > ->
      t
    val create_move :
      Hex.MHex.axial_coord ->
      Hex.MHex.axial_coord -> Layer_enum.MLayer_enum.t -> t
    val create_attack :
      Hex.MHex.axial_coord ->
      Hex.MHex.axial_coord ->
      Layer_enum.MLayer_enum.t -> Layer_enum.MLayer_enum.t -> t
    val create_pass : Hex.MHex.axial_coord -> Layer_enum.MLayer_enum.t -> t
    val create_spawn_entity :
      Hex.MHex.axial_coord ->
      Hex.MHex.axial_coord -> Entity_enum.MEntity_enum.t -> t
    val create_use_item : Item.MItem.code -> Item.MItem.param -> t
    val create_pickup_item :
      Hex.MHex.axial_coord ->
      Hex.MHex.axial_coord -> Layer_enum.MLayer_enum.t -> t
  end
