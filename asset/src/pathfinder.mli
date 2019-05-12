module MPathfinder :
  sig
    exception No_path_found
    val dijkstra_reachable :
      Tile.MTile.tile ->
      Tile.MTile.tile ->
      Grid.MGrid.t -> int -> Layer_enum.MLayer_enum.t -> Tile.MTile.tile list
    val a_star_path_to :
      Tile.MTile.t ->
      Tile.MTile.t ->
      Grid.MGrid.t -> Layer_enum.MLayer_enum.t -> Tile.MTile.t list * int
    val a_star_next_move :
      Tile.MTile.t ->
      Tile.MTile.t ->
      Grid.MGrid.t -> Layer_enum.MLayer_enum.t -> Tile.MTile.t
  end
