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

  (* Defines the result of each action *)
  type res = {
    added : MEntity.t list;
    deleted : MEntity.t list;
    animation : MAnimation.t;
  }

  let empty = {
    added = [];
    deleted = [];
    animation = MAnimation.create []
  }

  let get_added res = res.added
  let get_deleted res = res.deleted
  let get_animation res = res.animation

  let add r1 r2 = 
    {
      added = r1.added @ r2.added;
      deleted = r1.deleted @ r2.deleted;
      animation = MAnimation.add r1.animation r2.animation;
    }

  let attack grid src dst = 
    let sr,sq,dr,dq =
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
        {
          added = [];
          deleted = [dmu];
          animation = MAnimation.create []
        }
      end
      (*if the entity isn't dead*)
    else
      let new_dmu = dmu#remove_hp damage
      in 
      MGrid.remove_mg_at grid dr dq;
      MGrid.set_mg_at grid dr dq new_dmu;
      {
        added = [new_dmu];
        deleted = [dmu];
        animation = MAnimation.create []
      }

  (* Refill a unit movement point *)
  let refill_mp grid src dst =
    let sr,sq = MHex.get_r src,MHex.get_q src
    in
    let mu = MGrid.get_mg_at grid sr sq in
    let new_mu = mu#refill_mp in
    let () =
      MGrid.remove_mg_at grid sr sq;
      MGrid.set_mg_at grid sr sq new_mu;
    in
    {
      added = [new_mu];
      deleted = [mu];
      animation = MAnimation.create []
    }

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
      let ent = MGrid.get_mg_at grid sr sq in
      let old_ent,new_ent = 
        ent,ent#move dr dq
      in

      let start = MGrid.get_tile old_ent#get_r old_ent#get_q grid in
      let goal = MGrid.get_tile new_ent#get_r new_ent#get_q grid in
      let path_taken,mv_cost = MPathfinder.dijkstra_path start goal grid old_ent#get_current_mp in
      let movement_animation_list =
        List.fold_left (
          fun acc x -> 
            let new_ent = (old_ent#move x#get_r x#get_q)#set_status MEntity.MOVING in
            ((new_ent),10,10) :: acc
        ) [] (List.rev path_taken)
      in
      let new_ent_minus_current_mp = new_ent#remove_mp mv_cost in
      let () =
        MGrid.remove_mg_at grid sr sq;
        MGrid.set_mg_at grid dr dq new_ent_minus_current_mp;
      in
      {
        added = [new_ent_minus_current_mp];
        deleted = [old_ent];
        animation = MAnimation.create [movement_animation_list]
      }

  exception No_action_specified

  type t = {
    code : MAction_enum.t;
    src : MHex.axial_coord;
    dst : MHex.axial_coord;
  }

  let get_code t = t.code
  let get_src t = t.src
  let get_dst t = t.dst

  let create code src dst = {
    code = code;
    src = src;
    dst = dst
  }

  let execute t grid =
    match t with
    | None -> raise No_action_specified
    | Some e ->
      let src = get_src e in
      let dst = get_dst e in
      match get_code e with
      | MAction_enum.MOVE -> move grid src dst
      | MAction_enum.ATTACK -> attack grid src dst
      | MAction_enum.REFILL_MP -> refill_mp grid src dst
      | _ -> raise Not_yet_implemented

end
;;