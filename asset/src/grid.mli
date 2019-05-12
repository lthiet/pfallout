module MGrid :
  sig
    type t = {
      level_radius : int;
      tile_grid : Tile.MTile.tile array array;
      item_grid : Item.MItem.t option array array;
      military_grid : Entity.MEntity.t option array array;
      infrastructure_grid : Entity.MEntity.t option array array;
    }
    val remove_item_at : t -> int -> int -> unit
    exception No_item
    val get_item_at_ax : t -> Hex.MHex.axial_coord -> Item.MItem.t
    val add_item_at : t -> Item.MItem.t -> unit
    val empty_at : t -> int -> int -> Layer_enum.MLayer_enum.t -> bool
    val remove_at : t -> int -> int -> Layer_enum.MLayer_enum.t -> unit
    exception Grid_cell_no_entity
    val get_at :
      t -> int -> int -> Layer_enum.MLayer_enum.t -> Entity.MEntity.t
    val get_at_ax :
      t ->
      Hex.MHex.axial_coord -> Layer_enum.MLayer_enum.t -> Entity.MEntity.t
    val set_at :
      t ->
      int ->
      int ->
      < add_hp_max : int -> Entity.MEntity.entity;
        add_item_to_inventory : Item.MItem.t -> Entity.MEntity.entity;
        can_move : bool; check_layer : Layer_enum.MLayer_enum.t -> bool;
        check_status : Entity.MEntity.status -> bool;
        check_unit_type : Entity.MEntity.unit_type -> bool;
        empty_mp : Entity.MEntity.entity;
        get_aos : Action_enum.MAction_enum.enum list; get_ap : int;
        get_ar : int; get_at : Entity.MEntity.attack_type; get_atks : 
        int; get_axial : Hex.MHex.axial_coord;
        get_behaviour : Behaviour_enum.MBehaviour_enum.t;
        get_box : Tsdl.Sdl.rect; get_cube : Hex.MHex.cube_coord;
        get_defs : int; get_faction : Faction_enum.MFaction_enum.t;
        get_hp : int; get_id : int; get_inventory : Inventory.MInventory.t;
        get_lt : Entity.MEntity.layer_type; get_mp : int;
        get_pa : Action_enum.MAction_enum.enum list; get_pc : int;
        get_q : int; get_r : int; get_status : Entity.MEntity.status;
        get_tt : Entity.MEntity.terrain_type;
        get_ut : Entity.MEntity.unit_type; get_x : int; get_y : int;
        get_z : int; is_dead : bool; is_low_hp : bool;
        move : int -> int -> Entity.MEntity.entity;
        refill_mp : Entity.MEntity.entity;
        remove_hp : int -> Entity.MEntity.entity;
        remove_mp : int -> Entity.MEntity.entity;
        set_behaviour : Behaviour_enum.MBehaviour_enum.t ->
                        Entity.MEntity.entity;
        set_inventory : Inventory.MInventory.t -> Entity.MEntity.entity;
        set_status : Entity.MEntity.status -> Entity.MEntity.entity > ->
      Layer_enum.MLayer_enum.t -> unit
    val add_at :
      t ->
      < add_hp_max : int -> Entity.MEntity.entity;
        add_item_to_inventory : Item.MItem.t -> Entity.MEntity.entity;
        can_move : bool; check_layer : Entity.MEntity.layer_type -> bool;
        check_status : Entity.MEntity.status -> bool;
        check_unit_type : Entity.MEntity.unit_type -> bool;
        empty_mp : Entity.MEntity.entity;
        get_aos : Action_enum.MAction_enum.enum list; get_ap : int;
        get_ar : int; get_at : Entity.MEntity.attack_type; get_atks : 
        int; get_axial : Hex.MHex.axial_coord;
        get_behaviour : Behaviour_enum.MBehaviour_enum.t;
        get_box : Tsdl.Sdl.rect; get_cube : Hex.MHex.cube_coord;
        get_defs : int; get_faction : Faction_enum.MFaction_enum.t;
        get_hp : int; get_id : int; get_inventory : Inventory.MInventory.t;
        get_lt : Entity.MEntity.layer_type; get_mp : int;
        get_pa : Action_enum.MAction_enum.enum list; get_pc : int;
        get_q : int; get_r : int; get_status : Entity.MEntity.status;
        get_tt : Entity.MEntity.terrain_type;
        get_ut : Entity.MEntity.unit_type; get_x : int; get_y : int;
        get_z : int; is_dead : bool; is_low_hp : bool;
        move : int -> int -> Entity.MEntity.entity;
        refill_mp : Entity.MEntity.entity;
        remove_hp : int -> Entity.MEntity.entity;
        remove_mp : int -> Entity.MEntity.entity;
        set_behaviour : Behaviour_enum.MBehaviour_enum.t ->
                        Entity.MEntity.entity;
        set_inventory : Inventory.MInventory.t -> Entity.MEntity.entity;
        set_status : Entity.MEntity.status -> Entity.MEntity.entity > ->
      unit
    val get_tile : int -> int -> t -> Tile.MTile.tile
    val get_tile_ax : Hex.MHex.axial_coord -> t -> Tile.MTile.tile
    val get_tile_cube : Hex.MHex.cube_coord -> t -> Tile.MTile.tile
    val create : int -> t
    val render :
      Tsdl.Sdl.renderer ->
      Texture_pack.MTexture_pack.textures ->
      t -> float -> Tsdl.Sdl.rect -> 'a -> unit
    type neighbours_t = {
      right : Tile.MTile.tile;
      top_right : Tile.MTile.tile;
      top_left : Tile.MTile.tile;
      left : Tile.MTile.tile;
      bot_right : Tile.MTile.tile;
      bot_left : Tile.MTile.tile;
    }
    val neighbours_list :
      < get_cube : Hex.MHex.cube_coord; .. > -> t -> Tile.MTile.tile list
    val range_tile : t -> Tile.MTile.tile -> int -> Tile.MTile.tile list
    val get_random_accessible_tile :
      t ->
      Layer_enum.MLayer_enum.t ->
      ?center:Hex.MHex.axial_coord -> ?bound:int -> unit -> Tile.MTile.tile
    val passable_tile_list :
      (< is_impassable : bool; .. > as 'a) list -> 'a list
    val free_tile_list :
      t ->
      Layer_enum.MLayer_enum.t ->
      (< get_q : int; get_r : int; .. > as 'a) list -> 'a list
    val nearby_enemies :
      t ->
      < get_faction : Faction_enum.MFaction_enum.t; get_q : int; get_r : 
        int; .. > ->
      int -> Layer_enum.MLayer_enum.t -> Entity.MEntity.t list
    val nearby_item_of_type :
      t ->
      Hex.MHex.axial_coord ->
      int -> Item.MItem.enum -> Tile.MTile.tile option
  end
