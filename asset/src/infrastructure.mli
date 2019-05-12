module MInfrastructure :
  sig
    val create_city :
      int -> int -> Faction_enum.MFaction_enum.t -> Entity.MEntity.entity
  end
