class virtual game_object id x y z =
	object (self)
		val id : int = id
		val x : int = x
		val y : int = y
		val z : int = z
		method get_x  = x
		method get_y  = y
		method get_z  = z
	end;;