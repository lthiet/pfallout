

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

		  
