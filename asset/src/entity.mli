module MEntity :
  sig
    type unit_type = Entity_enum.MEntity_enum.t
    type layer_type = Layer_enum.MLayer_enum.t
    type attack_type = MELEE | RANGED
    type terrain_type = GROUND | AIR
    type status = IDLE | ATTACKING | MOVING
    class entity :
      int ->
      int ->
      int ->
      int ->
      int ->
      int ->
      int ->
      int ->
      int ->
      Action_enum.MAction_enum.enum list ->
      Action_enum.MAction_enum.enum list ->
      Faction_enum.MFaction_enum.t ->
      unit_type ->
      layer_type ->
      attack_type ->
      terrain_type ->
      int ->
      Behaviour_enum.MBehaviour_enum.t ->
      object ('a)
        val aos : Action_enum.MAction_enum.enum list
        val ap : int
        val ar : int
        val at : attack_type
        val atks : int
        val axial_coord : Hex.MHex.axial_coord
        val behaviour : Behaviour_enum.MBehaviour_enum.t
        val defs : int
        val faction : Faction_enum.MFaction_enum.t
        val hp : int
        val id : int
        val inventory : Inventory.MInventory.t
        val lt : layer_type
        val mp : int
        val pa : Action_enum.MAction_enum.enum list
        val pc : int
        val status : status
        val truc : unit
        val tt : terrain_type
        val ut : unit_type
        method add_hp_max : int -> 'a
        method add_item_to_inventory : Item.MItem.t -> 'a
        method can_move : bool
        method check_layer : layer_type -> bool
        method check_status : status -> bool
        method check_unit_type : unit_type -> bool
        method empty_mp : 'a
        method get_aos : Action_enum.MAction_enum.enum list
        method get_ap : int
        method get_ar : int
        method get_at : attack_type
        method get_atks : int
        method get_axial : Hex.MHex.axial_coord
        method get_behaviour : Behaviour_enum.MBehaviour_enum.t
        method get_box : Tsdl.Sdl.rect
        method get_cube : Hex.MHex.cube_coord
        method get_defs : int
        method get_faction : Faction_enum.MFaction_enum.t
        method get_hp : int
        method get_id : int
        method get_inventory : Inventory.MInventory.t
        method get_lt : layer_type
        method get_mp : int
        method get_pa : Action_enum.MAction_enum.enum list
        method get_pc : int
        method get_q : int
        method get_r : int
        method get_status : status
        method get_tt : terrain_type
        method get_ut : unit_type
        method get_x : int
        method get_y : int
        method get_z : int
        method is_dead : bool
        method is_low_hp : bool
        method move : int -> int -> 'a
        method refill_mp : 'a
        method remove_hp : int -> 'a
        method remove_mp : int -> 'a
        method set_behaviour : Behaviour_enum.MBehaviour_enum.t -> 'a
        method set_inventory : Inventory.MInventory.t -> 'a
        method set_status : status -> 'a
      end
    type t = entity
    val create :
      int ->
      int ->
      int ->
      int ->
      int ->
      int ->
      int ->
      int ->
      Action_enum.MAction_enum.enum list ->
      Action_enum.MAction_enum.enum list ->
      Faction_enum.MFaction_enum.t ->
      unit_type ->
      layer_type ->
      attack_type ->
      terrain_type -> int -> Behaviour_enum.MBehaviour_enum.t -> entity
    val create_fx_binder : unit -> entity
    val is_infrastructure :
      < get_ut : Entity_enum.MEntity_enum.t; .. > -> bool
    val is_military : < get_ut : Entity_enum.MEntity_enum.t; .. > -> bool
    val to_string :
      < get_axial : Hex.MHex.axial_coord;
        get_behaviour : Behaviour_enum.MBehaviour_enum.t; get_hp : int;
        get_inventory : Inventory.MInventory.t;
        get_lt : Layer_enum.MLayer_enum.t; get_mp : int; .. > ->
      string
    val entity_textures :
      < get_faction : Faction_enum.MFaction_enum.code * 'a;
        get_ut : Entity_enum.MEntity_enum.t; .. > ->
      Texture_pack.MTexture_pack.textures -> Texture_wrapper.MTexture.t
    val get_clip : int -> < get_status : status; .. > -> Tsdl.Sdl.rect
    exception Option_coord_need_to_be_both_none_or_some
    val render :
      Tsdl.Sdl.renderer ->
      ?x:int option ->
      ?y:int option ->
      < get_axial : Hex.MHex.axial_coord; get_box : Tsdl.Sdl.rect;
        get_faction : Faction_enum.MFaction_enum.code * 'a;
        get_status : status; get_ut : Entity_enum.MEntity_enum.t; .. > ->
      Texture_pack.MTexture_pack.textures ->
      float -> Tsdl.Sdl.rect -> int -> int * int * bool
  end
