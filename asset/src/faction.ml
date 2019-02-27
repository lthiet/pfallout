open Entity
open Faction_enum

module MFaction =
struct

  type t = {
    code : MFaction_enum.t;
    entities_list : MEntity.t list;
  }

  let to_string t = 
    let tmp1 = MFaction_enum.to_string t.code in
    let tmp2 =
      List.fold_left (
        fun acc x ->
          acc ^ (MEntity.to_string x) ^ " "
      ) "" t.entities_list
    in
    tmp1 ^ "\n" ^ tmp2 ^ "\n"

  let create_faction code =
    {
      code = code;
      entities_list = [];
    }

  let equal t1 t2 =
    t1.code = t2.code

  let add_entity m t =
    {
      t with
      entities_list = m :: t.entities_list
    }

  let get_entity t = t.entities_list

  let get_code t = t.code 

  let entity mu t =
    let f = mu#get_faction in
    f = t.code 

  let update_entities t added deleted =
    let rec aux l acc added deleted =
      match l with
      | [] ->
        List.fold_left (fun acc x -> 
            if entity x t then
              x :: acc
            else
              acc
          ) acc added
      | x :: s ->
        let new_acc =
          if List.exists (fun y -> x = y) deleted then
            acc
          else
            x :: acc
        in
        aux s new_acc added deleted
    in
    let new_ml = aux t.entities_list [] added deleted in
    {
      t with
      entities_list = new_ml
    }
end
;;