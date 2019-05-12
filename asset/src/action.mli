module MAction :
  sig
    type res = {
      added : Entity.MEntity.t list;
      deleted : Entity.MEntity.t list;
      animation : Animation.MAnimation.t;
    }
    val empty : res
    val get_added : res -> Entity.MEntity.t list
    val get_deleted : res -> Entity.MEntity.t list
    val get_animation : res -> Animation.MAnimation.t
    val add : res -> res -> res
    val change_behaviour :
      Grid.MGrid.t ->
      Hex.MHex.axial_coord ->
      Entity.MEntity.layer_type -> Behaviour_enum.MBehaviour_enum.t -> res
    val execute : Action_enum.MAction_enum.t option -> Grid.MGrid.t -> res
  end
