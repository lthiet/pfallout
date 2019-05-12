module MBehaviour :
  sig
    val change_behaviour :
      Grid.MGrid.t ->
      (< get_axial : Hex.MHex.axial_coord;
         get_behaviour : Behaviour_enum.MBehaviour_enum.t;
         get_faction : Faction_enum.MFaction_enum.t;
         get_inventory : Inventory.MInventory.t;
         get_lt : Layer_enum.MLayer_enum.t; get_q : int; get_r : int;
         is_low_hp : bool;
         set_behaviour : Behaviour_enum.MBehaviour_enum.t -> 'a; .. >
       as 'a) ->
      Behaviour_enum.MBehaviour_enum.t
    val compute_behaviour :
      Grid.MGrid.t ->
      < can_move : bool; get_axial : Hex.MHex.axial_coord;
        get_behaviour : Behaviour_enum.MBehaviour_enum.t;
        get_faction : Faction_enum.MFaction_enum.t;
        get_lt : Layer_enum.MLayer_enum.t; get_mp : int; get_q : int;
        get_r : int; .. > ->
      Action_enum.MAction_enum.t
  end
