open Item

(* This module implement the inventory of a character *)
module MInventory = struct
  type t = {
    item_list : MItem.t list
  }

  let to_string t =
    List.fold_left ( fun acc x -> acc ^ MItem.to_string x ) "" t.item_list

  let empty = {
    item_list = []
  }

  (* Returns the item list of an inventory *)
  let get_item_list inventory = inventory.item_list

  let add_item t item =
    {
      item_list = item :: t.item_list
    }

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

  (* Return the first item (and removes it from the list) that is same as code specified in parameters,
      for example if enum = HEALTHPACK_E then returns the first healthpack found, otherwise, return none *)
  let fetch_item inventory code = 
    let l = get_item_list inventory in
    (* For each item in the inventory check the item code *)
    let res_item,res_list =
      List.fold_left (
        fun acc x -> 
          (* Fetch the currently known item and list *)
          let acc_item,acc_list = acc in
          (* Fetch the new item *)
          let new_item,new_list = 
            match acc_item with
            (* The item is already found *)
            | Some _ -> acc_item,(x :: acc_list)
            (* The item is not found, check if the new one is the item *)
            | None ->
              if x#get_code = code then
                (Some x),(acc_list)
              else
                None,(x :: acc_list)
          in
          new_item,new_list
      ) (None,[]) l
    in
    res_item,{item_list = res_list}

end
