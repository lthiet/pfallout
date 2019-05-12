module MFaction :
  sig
    type t = {
      code : Faction_enum.MFaction_enum.t;
      entities_list : Entity.MEntity.t list;
    }
    val to_string : t -> string
    val create_faction : Faction_enum.MFaction_enum.t -> t
    val equal : t -> t -> bool
    val add_entity : Entity.MEntity.t -> t -> t
    val get_entity : t -> Entity.MEntity.t list
    val get_code : t -> Faction_enum.MFaction_enum.t
    val entity_in :
      < get_faction : Faction_enum.MFaction_enum.t; .. > -> t -> bool
    val update_entities :
      t -> Entity.MEntity.t list -> Entity.MEntity.t list -> t
    val faction_can_play : t -> bool
  end
