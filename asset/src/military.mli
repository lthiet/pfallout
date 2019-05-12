module MMilitary :
  sig
    val create_soldier :
      int -> int -> Faction_enum.MFaction_enum.t -> Entity.MEntity.entity
  end
