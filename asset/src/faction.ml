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
    controlled_by_player : bool
  }

  let create_faction code cbp =
    {
      code = code;
      military_list = [];
      infrastructure_list = [];
      controlled_by_player = cbp
    }

  let add_military t m =
    {
      t with
      military_list = m :: t.military_list
    }

  let get_military t = t.military_list

  let update_military t added deleted =
    let rec aux l acc added deleted =
      match l with
      | [] ->
        added @ acc
      | x :: s ->
        let new_acc =
          if List.exists (fun y -> x = y ) deleted then
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