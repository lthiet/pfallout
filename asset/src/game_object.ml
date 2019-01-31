open Hex

class virtual game_object id r q =
	object (self)
		val id : int = id
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
	end;;