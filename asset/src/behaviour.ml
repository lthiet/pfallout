open Grid
open Entity
open Action
open Action_enum
open Utils
open Behaviour_enum
open Pathfinder

(* This module will compute the behaviour of adversary entities. 
   In general, it will return an action signature type *)
module MBehaviour = struct


  let move_to_random_location grid entity = 
    let tile_src = MGrid.get_tile entity#get_r entity#get_q grid in
    (* NB : djikstra reachable doesnt care about dst *)
    try 
      let new_tile = 
        (* Get all the reachable tiles *)
        let reachable = MPathfinder.dijkstra_reachable tile_src tile_src grid entity#get_current_mp entity#get_lt |> MGrid.free_tile_list grid entity#get_lt in

        (* Recursively get tile that are empty *)
        let rec aux () =
          let res = random_elem_list reachable in
          if MGrid.empty_at grid res#get_r res#get_q entity#get_lt then
            res
          else
            aux ()
        in
        aux ()
        (* If there is nowherer else to move, pass the turn *)
      in
      MAction.create MAction_enum.MOVE entity#get_axial new_tile#get_axial
    with Empty_list ->
      MAction.create MAction_enum.PASS entity#get_axial entity#get_axial

  let attack_nearby_enemy grid entity = 
    let neighbouring_enemy = MGrid.nearby_enemy grid entity 1 entity#get_lt in
    match neighbouring_enemy with
    | None ->
      begin
        let nearest_enemy = MGrid.nearby_enemy grid entity 6 entity#get_lt in
        match nearest_enemy with
        | None -> move_to_random_location grid entity
        | Some x ->
          let src = MGrid.get_tile entity#get_r entity#get_q grid in
          try
            let dst = MGrid.get_tile x#get_r x#get_q grid in
            let closest_dst = MPathfinder.closest_tile src dst grid entity#get_lt in
            if closest_dst = src then
              MAction.create MAction_enum.PASS entity#get_axial entity#get_axial
            else
              MAction.create MAction_enum.MOVE entity#get_axial closest_dst#get_axial
          with MPathfinder.No_path_found ->
            MAction.create MAction_enum.PASS entity#get_axial entity#get_axial
      end
    | Some x ->
      MAction.create MAction_enum.ATTACK entity#get_axial x#get_axial

  let change_behaviour grid entity =
    match entity#get_behaviour with
    | MBehaviour_enum.WANDERING -> 
      begin
        let nearby_enemy = MGrid.nearby_enemy grid entity 4 entity#get_lt in
        match nearby_enemy with
        | None -> MBehaviour_enum.WANDERING
        | Some x -> MBehaviour_enum.ATTACKING
      end
    | MBehaviour_enum.ATTACKING ->
      begin
        let nearby_enemy = MGrid.nearby_enemy grid entity 4 entity#get_lt in
        match nearby_enemy with
        | None -> MBehaviour_enum.WANDERING
        | Some x -> MBehaviour_enum.ATTACKING
      end
    | _ -> raise Not_yet_implemented


  (* Main function *)
  let compute_behaviour grid entity = 
    match entity#get_behaviour with
    | MBehaviour_enum.WANDERING ->
      move_to_random_location grid entity
    | MBehaviour_enum.ATTACKING ->
      attack_nearby_enemy grid entity
    | _ -> raise Not_yet_implemented

end
