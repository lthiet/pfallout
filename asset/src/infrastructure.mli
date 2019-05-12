module MInfrastructure :
  sig
    val create_city :
      int -> int -> Faction_enum.MFaction_enum.t -> bool -> Entity.MEntity.entity
  end
