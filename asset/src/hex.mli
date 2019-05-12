module MHex :
sig
  val size : int
  val width : int
  val height : int
  type axial_coord = { q : int; r : int; }
  val axial_to_string : axial_coord -> string
  val create_ax : int -> int -> axial_coord
  type cube_coord = { x : int; y : int; z : int; }
  val get_r : axial_coord -> int
  val get_q : axial_coord -> int
  val get_x : cube_coord -> int
  val get_y : cube_coord -> int
  val get_z : cube_coord -> int
  val to_string_ax : axial_coord -> string
  val axial_to_cube : axial_coord -> cube_coord
  val cube_to_axial : cube_coord -> axial_coord
  val axial_to_screen_coord : axial_coord -> int * int
  type neighbours_t = {
    left : cube_coord;
    right : cube_coord;
    top_left : cube_coord;
    top_right : cube_coord;
    bot_left : cube_coord;
    bot_right : cube_coord;
  }
  val neighbours : cube_coord -> neighbours_t
  val range_ax : int -> axial_coord -> cube_coord list
  val dist_cube : cube_coord -> cube_coord -> int
  val pixel_to_ax : int -> int -> axial_coord
end
