module MTile :
  sig
    type terrain_feature = MOUNTAIN | HILL | FOREST | LAKE | REGULAR
    val int_to_terrain_feature : int -> terrain_feature
    type tile_type = TILE_GRASSLAND | TILE_DESERT | TILE_SNOW
    val int_to_tile_type : int -> tile_type
    class tile :
      int ->
      int ->
      tile_type ->
      terrain_feature ->
      object ('a)
        val axial_coord : Hex.MHex.axial_coord
        val terrain_feature : terrain_feature
        val tile_type : tile_type
        method get_axial : Hex.MHex.axial_coord
        method get_box : Tsdl.Sdl.rect
        method get_cube : Hex.MHex.cube_coord
        method get_movement_cost : int
        method get_q : int
        method get_r : int
        method get_terrain_feature : terrain_feature
        method get_tile_type : tile_type
        method get_x : int
        method get_y : int
        method get_z : int
        method is_forest : bool
        method is_hill : bool
        method is_impassable : bool
        method is_lake : bool
        method is_mountain : bool
        method is_regular : bool
        method move : int -> int -> 'a
      end
    type t = tile
    val compare :
      < get_q : int; get_r : int; .. > ->
      < get_q : int; get_r : int; .. > -> int
    val render :
      Tsdl.Sdl.renderer ->
      < get_axial : Hex.MHex.axial_coord; get_box : Tsdl.Sdl.rect;
        get_terrain_feature : terrain_feature; get_tile_type : tile_type;
        is_lake : bool; .. > ->
      Texture_wrapper.MTexture.t ->
      Texture_wrapper.MTexture.t -> float -> Tsdl.Sdl.rect -> unit
  end
