module MEntity_information :
  sig
    type basic = {
      id : int;
      x : int;
      y : int;
      displayed : bool;
      healthbar : Healthbar.MHealthbar.t;
      mp_left : int;
      faction_enum : Faction_enum.MFaction_enum.t;
    }
    val get_info :
      < get_faction : Faction_enum.MFaction_enum.t; get_hp : int;
        get_id : int; get_mp : int; get_ut : Entity_enum.MEntity_enum.t; .. > ->
      int -> int -> bool -> basic option
    val render : Tsdl.Sdl.renderer -> 'a -> float -> basic -> unit
  end
