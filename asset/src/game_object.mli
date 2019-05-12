class virtual game_object :
  int ->
  int ->
  object ('a)
    val axial_coord : Hex.MHex.axial_coord
    method get_axial : Hex.MHex.axial_coord
    method get_box : Tsdl.Sdl.rect
    method get_cube : Hex.MHex.cube_coord
    method get_q : int
    method get_r : int
    method get_x : int
    method get_y : int
    method get_z : int
    method move : int -> int -> 'a
  end
