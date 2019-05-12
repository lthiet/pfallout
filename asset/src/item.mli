module MItem :
  sig
    type code = HEALTHPACK of int | NUKE of int
    type enum = HEALTHPACK_E | NUKE_E
    val to_string_enum : enum -> string
    type param =
        HEALTHPACK_P of Hex.MHex.axial_coord * Hex.MHex.axial_coord *
          Layer_enum.MLayer_enum.t
      | NUKE_P of Hex.MHex.axial_coord * Hex.MHex.axial_coord *
          Layer_enum.MLayer_enum.t
    val create_healthpack_param :
      Hex.MHex.axial_coord ->
      Hex.MHex.axial_coord -> Layer_enum.MLayer_enum.t -> param
    val create_nuke_param :
      Hex.MHex.axial_coord ->
      Hex.MHex.axial_coord -> Layer_enum.MLayer_enum.t -> param
    val same_code_and_enum : code -> enum -> bool
    class item :
      int ->
      int ->
      code ->
      object ('a)
        val axial_coord : Hex.MHex.axial_coord
        val code : code
        val owned : bool
        method get_axial : Hex.MHex.axial_coord
        method get_box : Tsdl.Sdl.rect
        method get_code : code
        method get_cube : Hex.MHex.cube_coord
        method get_q : int
        method get_r : int
        method get_x : int
        method get_y : int
        method get_z : int
        method is_owned : bool
        method move : int -> int -> 'a
        method set_owned : bool -> 'a
      end
    type t = item
    exception Incorrect_Code
    val get_amount_of_healthpack : < get_code : code; .. > -> int
    val get_radius_of_nuke : < get_code : code; .. > -> int
    val to_string : < get_code : code; is_owned : bool; .. > -> string
    val create_healthpack : int -> int -> int -> item
    val create_nuke : int -> int -> int -> item
    val render :
      Tsdl.Sdl.renderer ->
      < get_axial : Hex.MHex.axial_coord; get_box : Tsdl.Sdl.rect;
        get_code : code; is_owned : bool; .. > ->
      Texture_pack.MTexture_pack.textures ->
      float -> Tsdl.Sdl.rect -> 'a -> unit
  end
