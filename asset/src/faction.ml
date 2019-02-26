open Entity
open Military
open Infrastructure
open Faction_enum

module MFaction =
struct

  type t = {
    code : MFaction_enum.t;
    military_list : MMilitary.t list;
    infrastructure_list : MInfrastructure.t list;
  }

  let to_string t = 
    let tmp1 = MFaction_enum.to_string t.code in
    let tmp2 =
      List.fold_left (
        fun acc x ->
          acc ^ (MMilitary.to_string x) ^ " "
      ) "" t.military_list
    in
    tmp1 ^ "\n" ^ tmp2 ^ "\n"

  let create_faction code =
    {
      code = code;
      military_list = [];
      infrastructure_list = [];
    }

  let equal t1 t2 =
    t1.code = t2.code

  let add_military m t =
    {
      t with
      military_list = m :: t.military_list
    }

  let get_military t = t.military_list

  let get_code t = t.code 

  let military_in mu t =
    let f = mu#get_faction in
    f = t.code 

  let update_military t added deleted =
    let rec aux l acc added deleted =
      match l with
      | [] ->
        List.fold_left (fun acc x -> 
            if military_in x t then
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
    let new_ml = aux t.military_list [] added deleted in
    {
      t with
      military_list = new_ml
    }

  let add_infrastructure t i =
    {
      t with
      infrastructure_list = i :: t.infrastructure_list
    }

  let get_infrastructure t = t.infrastructure_list
end
;;