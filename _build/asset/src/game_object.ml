open Hex
open Tsdl

class virtual game_object r q =
  object (self)
    val axial_coord : MHex.axial_coord = {
      q = q;
      r = r;
    }
    method get_axial = axial_coord
    method get_cube = MHex.axial_to_cube axial_coord
    method get_x  = 
      let cube = self#get_cube in
      cube.x

    method get_y  = 
      let cube = self#get_cube in
      cube.y

    method get_z  = 
      let cube = self#get_cube in
      cube.z

    method get_q = axial_coord.q

    method get_r = axial_coord.r

    method get_box = 
      let ax = self#get_axial in 
      let x,y = MHex.axial_to_screen_coord ax in
      Sdl.Rect.create x y MHex.width MHex.height

    method move r q = 
      {< axial_coord = {
             q = q;
             r = r
           } >}
  end;;