open Grid
open Entity
open Action
open Action_enum
open Utils
open Behaviour_enum
open Pathfinder
open Military
open Layer_enum
open Entity_enum
open Hex
open Item

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
      in
      MAction_enum.create_move entity#get_axial new_tile#get_axial entity#get_lt
    with Empty_list ->
      (* If there is nowherer else to move, pass the turn *)
      MAction_enum.create_pass entity#get_axial entity#get_lt

  let attack_nearby_enemy grid entity target range  = 
    (* Look for enemies in the nearest vicinity *)
    let neighbouring_enemies = MGrid.nearby_enemies grid entity 1 entity#get_lt in

    match neighbouring_enemies with
    (* There is no enemy in the nearest vicinity, we will look further *)
    | [] ->
      begin
        (* Look for all the enemies in the range of the attack behaviour *)
        let nearest_enemies = MGrid.nearby_enemies grid entity range entity#get_lt in
        match nearest_enemies with
        (* There are no enemies, we keep wandering *)
        | [] -> move_to_random_location grid entity
        | x :: s ->
          (* Look if the target is among the nearest enemies *)
          let nearest_target =
            List.fold_left (
              fun acc x -> 
                if x#get_id = target then
                  Some x
                else
                  None
            ) None nearest_enemies
          in
          match nearest_target with
          (* The target is found, go to it*)
          | Some target ->
            begin
              let src = MGrid.get_tile entity#get_r entity#get_q grid in
              try
                let dst = MGrid.get_tile target#get_r target#get_q grid in
                let closest_dst = MPathfinder.closest_tile src dst grid entity#get_lt in
                if closest_dst = src then
                  MAction_enum.create_pass entity#get_axial entity#get_lt
                else
                  MAction_enum.create_move entity#get_axial closest_dst#get_axial entity#get_lt
              with MPathfinder.No_path_found ->
                MAction_enum.create_pass entity#get_axial entity#get_lt
            end
          (* The target wasn't found, go to someone at random *)
          | None ->
            let src = MGrid.get_tile entity#get_r entity#get_q grid in
            try
              let dst = MGrid.get_tile x#get_r x#get_q grid in
              let closest_dst = MPathfinder.closest_tile src dst grid entity#get_lt in
              if closest_dst = src then
                MAction_enum.create_pass entity#get_axial entity#get_lt
              else
                MAction_enum.create_move entity#get_axial closest_dst#get_axial entity#get_lt
            with MPathfinder.No_path_found ->
              MAction_enum.create_pass entity#get_axial entity#get_lt
      end
    (* There are enemies around, we will check if it is our target *)
    | x :: s ->
      (* Among all the found enemies, check where is our target *)
      let neighbouring_target = 
        List.fold_left (
          fun acc x ->
            match acc with
            (* The target hasnt been found yet, we look for it *)
            | None ->
              (* The target has just been found, we update the acc *)
              if x#get_id = target then
                Some x
                (* Not the target, keep looking *)
              else
                None
            (* The target has been already found, we do not need to look further *)
            | _ -> acc
        ) None neighbouring_enemies
      in
      let enemy_to_attack = 
        match neighbouring_target with
        (* The target is found, attack it *)
        | Some target ->

          target
        (* The target is not found, but there is an enemy nearby, we attack it anyway *)
        | None -> 
          x
      in
      MAction_enum.create_attack entity#get_axial enemy_to_attack#get_axial entity#get_lt enemy_to_attack#get_lt

  let spawn_unit grid entity =
    let tile_below = MGrid.get_tile_ax entity#get_axial grid in
    (* Compute the tile next to it *)
    let tile_vicinity = MGrid.range_tile grid tile_below 1 in
    (* Compute the tile where it is possible to spawn a unit *)
    let passable_tile_vicinity = MGrid.passable_tile_list tile_vicinity in
    let free_tile = MGrid.free_tile_list grid MLayer_enum.MILITARY passable_tile_vicinity in
    (* Checks whether or not the list of free tile is enough
       to spawn a unit *)
    match free_tile with
    (* The list is empty, pass the turn *)
    | [] -> 
      MAction_enum.create_pass entity#get_axial entity#get_lt
    | x :: s -> 
      MAction_enum.create_spawn_entity entity#get_axial x#get_axial MEntity_enum.SOLDIER

  let go_to_healthpack grid entity dst =
    (* Get where the entity is *)
    let tile_src = MGrid.get_tile_ax entity#get_axial grid in
    (* Fetch the surrounding tiles *)
    let tile_vicinity = MGrid.range_tile grid tile_src 1 in
    (* Check whether or not the HP is found *)
    try
      let found_healthpack_tile = List.find ( fun x -> 
          try
            let found_item = MGrid.get_item_at_ax grid x#get_axial in
            MItem.same_code_and_enum found_item#get_code MItem.HEALTHPACK_E
          with MGrid.No_item | Invalid_argument _ -> false
        ) tile_vicinity
      in
      let found_healthpack = MGrid.get_item_at_ax grid found_healthpack_tile#get_axial in
      MAction_enum.create_use_item (found_healthpack#get_code) (MItem.create_healthpack_param entity#get_axial dst entity#get_lt)
    (* No healthpack found in the vicinity, we move towards it*)
    with Not_found ->
      let tile_dst = MGrid.get_tile_ax dst grid in
      try let closest_dst = MPathfinder.closest_tile tile_src tile_dst grid entity#get_lt in
        if closest_dst#get_axial = tile_src#get_axial then
          MAction_enum.create_pass entity#get_axial entity#get_lt
        else
          MAction_enum.create_move entity#get_axial closest_dst#get_axial entity#get_lt
      with MPathfinder.No_path_found ->
        MAction_enum.create_pass entity#get_axial entity#get_lt






  let change_behaviour grid entity =
    let not_currently_fleeing =
      match entity#get_behaviour with
      | MBehaviour_enum.FLEEING 
      | MBehaviour_enum.GOING_TO_HEALTHPACK _ -> false
      | _ -> true
    in
    if entity#is_low_hp && not_currently_fleeing then

      MBehaviour_enum.FLEEING
    else
      match entity#get_behaviour with
      | MBehaviour_enum.WANDERING -> 
        begin
          let nearby_enemy = MGrid.nearby_enemies grid entity 4 entity#get_lt in
          match nearby_enemy with
          | [] -> MBehaviour_enum.WANDERING
          |  x :: s -> MBehaviour_enum.ATTACKING (x#get_id,6)
        end
      | MBehaviour_enum.ATTACKING (target,range)  ->
        begin
          let nearby_enemies = MGrid.nearby_enemies grid entity range entity#get_lt in
          let b = 
            List.exists ( fun x -> x#get_id = target) nearby_enemies
          in
          if b then
            MBehaviour_enum.ATTACKING (target,6)
          else
            MBehaviour_enum.WANDERING
        end
      | MBehaviour_enum.SPAWNING -> MBehaviour_enum.SPAWNING
      | MBehaviour_enum.FLEEING -> 
        begin
          let healthpack = MGrid.nearby_item_of_type grid entity#get_axial 6 MItem.HEALTHPACK_E in
          match healthpack with
          | None -> MBehaviour_enum.FLEEING
          | Some x -> MBehaviour_enum.GOING_TO_HEALTHPACK(x#get_axial)
        end

      (* The entity is currently going to a healthpack *)
      | MBehaviour_enum.GOING_TO_HEALTHPACK(dst) -> 
        begin
          try
            (* The item is found, keep going *)
            let _ = MGrid.get_item_at_ax grid dst in
            MBehaviour_enum.GOING_TO_HEALTHPACK(dst)
          (* If the healthpack not longer exist, stop looking *)
          with MGrid.No_item -> 
            (* Continue to look for another one *)
            if entity#is_low_hp then
              MBehaviour_enum.FLEEING
              (* If already healed, do something else *)
            else
              MBehaviour_enum.WANDERING
        end
      | _ -> raise Not_yet_implemented


  (* Main function *)
  let compute_behaviour grid entity = 
    match entity#get_behaviour with
    | MBehaviour_enum.WANDERING
    | MBehaviour_enum.FLEEING ->
      move_to_random_location grid entity
    | MBehaviour_enum.ATTACKING (target,range)->
      attack_nearby_enemy grid entity target range
    | MBehaviour_enum.SPAWNING ->
      spawn_unit grid entity
    | MBehaviour_enum.GOING_TO_HEALTHPACK (dst) ->
      go_to_healthpack grid entity dst
    | _ -> raise Not_yet_implemented

end
