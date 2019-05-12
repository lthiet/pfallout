module MCursor :
  sig
    type status = SELECTING | SELECTING_DST | POSSIBLE | IMPOSSIBLE | HIDDEN
    class cursor :
      int ->
      int ->
      status ->
      object ('a)
        val axial_coord : Hex.MHex.axial_coord
        val status : status
        method get_axial : Hex.MHex.axial_coord
        method get_box : Tsdl.Sdl.rect
        method get_cube : Hex.MHex.cube_coord
        method get_q : int
        method get_r : int
        method get_status : status
        method get_x : int
        method get_y : int
        method get_z : int
        method is_hidden : bool
        method is_not_hidden : bool
        method move : int -> int -> 'a
        method set_status : status -> 'a
      end
    val create : int -> int -> status -> cursor
    val get_screen_x : < get_axial : Hex.MHex.axial_coord; .. > -> int
    val get_screen_y : < get_axial : Hex.MHex.axial_coord; .. > -> int
    val render :
      Tsdl.Sdl.renderer ->
      Texture_wrapper.MTexture.t ->
      < get_axial : Hex.MHex.axial_coord; get_status : status;
        is_not_hidden : bool; .. > ->
      float -> Tsdl.Sdl.rect -> unit
  end
