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
    let new_tile = 
      (* Get all the reachable tiles *)
      let reachable = MPathfinder.dijkstra_reachable tile_src tile_src grid entity#get_current_mp in

      (* Recursively get tile that are empty *)
      let rec aux () =
        let res = random_elem_list reachable in
        if MGrid.empty_mg_at grid res#get_r res#get_q then
          res
        else
          aux ()
      in
      aux ()
    in
    MAction.create MAction_enum.MOVE entity#get_axial new_tile#get_axial

  (* Main function *)
  let compute_behaviour grid entity = 
    match entity#get_behaviour with
    | MBehaviour_enum.WANDERING ->
      move_to_random_location grid entity
    | _ -> raise Not_yet_implemented

end
