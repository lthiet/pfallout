open Item

(* This module implement the inventory of a character *)
module MInventory = struct
  type t = {
    item_list : MItem.t list
  }

  (* Returns the item list of an inventory *)
  let get_item_list inventory = inventory.item_list

  (* Return the first item that is same as enum specified in parameters,
     for example if enum = HEALTHPACK_E then returns the first healthpack found, otherwise, return none *)
  let get_item inventory enum = 
    try
      let l = get_item_list inventory in
      (* For each item in the inventory check the item code *)
      let tmp = List.find ( fun x -> 
          let found_item_code = x#get_code in
          MItem.same_code_and_enum found_item_code enum
        ) l
      in Some tmp
    (* nothing was found, we return None *)
    with Not_found -> None
end
