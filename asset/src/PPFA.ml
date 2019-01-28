class virtual game_object =
	object (self)
		val virtual id : int
		val virtual pos_x : int
		val virtual pos_y : int
		val virtual pos_z : int
		method get_pos_x () = pos_x
		method get_pos_y () = pos_y
		method get_pos_z () = pos_z
	end;;
	

class virtual entity =
	object (self)
		inherit game_object 	
		val virtual health : int
		val virtual armor : int
		val virtual can_attack : bool
		val virtual can_move : bool
		val virtual action_point : int
		val virtual attack_str : int
		val virtual defense_str : int
		val virtual attack_range : int
(*
		val virtual item_list : item list
		val virtual faction: faction
*)
		method get_health () = health
		method get_attack_str () = attack_str
		method get_defense_str () = defense_str
		method get_attack_range () = attack_range
(*
		method get_item_list () = item_list
		method get_faction () = faction
*)
		
(*
		val virtual possible_action : Action list
		val mutable virtual action_on_start : Action list
		method execute_action_on_start () =
		method add_action_on_start a = 
*)
	end;;

class virtual produceable producer p_cost =
	object (self)
		inherit entity
		val produced_by = producer
		val prod_cost = p_cost
	end;;

		  
class tile id_init x y z t_type g m_cost =
	object (self)
		inherit game_object
		val id = id_init
		val pos_x = x
		val pos_y = y
		val pos_z = z
		val type_ = t_type
		val grid = g
		val mvmt_cost = m_cost
	end;;
	
(*
		method is_hidden = pas encore possible
*)
