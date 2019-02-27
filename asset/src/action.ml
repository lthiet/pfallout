open Grid
open Action_enum
open Pathfinder
open Hex
open Animation
open Entity
open Military
open Utils

(* This module computes actions in the game. Each action can modified the state
   of the game (the factions lists and the grid). Each action shall return the newly computed grid,
   the list of unit to delete, the list of unit to add *)
module MAction = struct
  exception Impossible_movement

  let attack grid src dst = 
    raise Not_yet_implemented
  (* let sr,sq,dr,dq =
     MHex.get_r src,
     MHex.get_q src,
     MHex.get_r dst,
     MHex.get_q dst
     in
     let smu, dmu = 
     MGrid.get_mg_at grid sr sq,
     MGrid.get_mg_at grid dr dq in
     let satks = smu#get_atks
     in
     let dhp,ddefs = dmu#get_hp,dmu#get_defs
     in
     let damage = if ((satks - ddefs)>0) then (satks-ddefs) else 0 in

     MGrid.remove_mg_at grid dr dq;
     MGrid.set_mg_at grid dr dq (dmu#remove_hp damage);

     (*if the entity is dead*)
     if (dhp<=damage) then 
     begin
      MGrid.remove_mg_at grid dr dq;
      [],[dmu],(MAnimation.create [])
     end
     (*if the entity isn't dead*)
     else
     let new_dmu = dmu#remove_hp damage
     in 
     MGrid.remove_mg_at grid dr dq;
     MGrid.set_mg_at grid dr dq new_dmu;
     [new_dmu],[dmu],(MAnimation.create []) *)

  (* Refill a unit movement point *)
  let refill_mp grid src dst =
    raise Not_yet_implemented
  (* let sr,sq = MHex.get_r src,MHex.get_q src
     in
     let mu = MGrid.get_mg_at grid sr sq in
     let new_mu = mu#refill_mp in
     let () =
     MGrid.remove_mg_at grid sr sq;
     MGrid.set_mg_at grid sr sq new_mu;
     in
     [new_mu],[mu],(MAnimation.create []) *)

  (* Move an entity that is on src to dst,
     returns an error if entity cannot
     be moved, ie does not have enough mp or the dst is
     an impassable terrain
  *)
  let move grid src dst =
    let sr,sq,dr,dq =
      MHex.get_r src,
      MHex.get_q src,
      MHex.get_r dst,
      MHex.get_q dst
    in
    let t_src,t_dst = 
      MGrid.get_tile sr sq grid,
      MGrid.get_tile dr dq grid
    in
    if t_dst#is_impassable then
      raise Impossible_movement
    else
      let ent = MGrid.get_ent_at grid sr sq in
      let old_ent,new_ent = 
        ent,ent#move dr dq
      in

      let start = MGrid.get_tile old_ent#get_r old_ent#get_q grid in
      let goal = MGrid.get_tile new_ent#get_r new_ent#get_q grid in
      let path_taken,mv_cost = MPathfinder.dijkstra_path start goal grid old_ent#get_current_mp in
      let movement_animation_list =
        List.fold_left (
          fun acc x -> (
              (old_ent#move x#get_r x#get_q),10) :: acc
        ) [] (List.rev path_taken)
      in
      let new_ent_minus_current_mp = new_ent#remove_mp mv_cost in
      let () =
        MGrid.remove_mg_at grid sr sq;
        MGrid.set_mg_at grid dr dq new_ent_minus_current_mp;
      in

      [new_ent_minus_current_mp],[old_ent],(MAnimation.create [movement_animation_list])

  exception No_action_specified

  let execute t grid src_ax dst_ax =
    match t with
    | None -> raise No_action_specified
    | Some e ->
      match e with
      | MAction_enum.MOVE -> move grid src_ax dst_ax
      | MAction_enum.ATTACK -> attack grid src_ax dst_ax
      | MAction_enum.REFILL_MP -> refill_mp grid src_ax dst_ax
      | _ -> raise Not_yet_implemented

end
;;