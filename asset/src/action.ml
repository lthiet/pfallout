open Grid
open Action_enum
open Pathfinder
open Hex
open Animation
open Entity
open Military
open Utils
open Behaviour_enum
open Layer_enum
open Entity_enum
open Entity_enum
open Item
open Inventory
open Fx

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

  exception Not_enough_point

  let attack grid src dst src_layer dst_layer = 
    let sr,sq,dr,dq =
      MHex.get_r src,
      MHex.get_q src,
      MHex.get_r dst,
      MHex.get_q dst
    in
    let smu, dmu = 
      MGrid.get_at grid sr sq src_layer,
      MGrid.get_at grid dr dq dst_layer in
    if smu#get_current_mp <= 0 then
      raise Not_enough_point
    else
      let satks = smu#get_atks
      in
      let dhp,ddefs = dmu#get_hp,dmu#get_defs
      in
      let damage = if ((satks - ddefs)>0) then (satks-ddefs) else 0 in

      let smu_without_mp = smu#empty_mp#remove_hp 10 in
      let () =
        MGrid.remove_at grid dr dq src_layer;
        MGrid.remove_at grid sr sq dst_layer;
        MGrid.add_at grid smu_without_mp;
        MGrid.add_at grid (dmu#remove_hp damage) 
      in

      let anim_src = [
        MAnimation.create_animation_unit (smu#set_status MEntity.ATTACKING) (None) 35
      ] 
      in
      let anim_dst = 
        let anim_dst_entity =
          dmu#set_status MEntity.ATTACKING
        in 
        [
          MAnimation.create_animation_unit anim_dst_entity (Some (MFx.create_from_entity anim_dst_entity MFx.ATTACKED)) 35
        ]
      in

      (*if the entity is dead*)
      if (dhp<=damage) then 
        begin
          let smu_on_top = smu_without_mp#move dr dq in
          let start = MGrid.get_tile_ax src grid in
          let goal = MGrid.get_tile_ax dst grid in
          let path_taken,mv_cost = MPathfinder.dijkstra_path start goal grid 2 src_layer in
          let movement_animation_list =
            List.fold_left (
              fun acc x -> 
                let au = MAnimation.create_animation_unit ((smu_without_mp#move x#get_r x#get_q)#set_status MEntity.MOVING) (None) 10 in
                au :: acc
            ) [] (List.rev path_taken)
          in

          let () =
            MGrid.remove_at grid dr dq dst_layer;
            MGrid.remove_at grid sr sq src_layer;
            MGrid.add_at grid smu_on_top
          in
          {
            added = [smu_on_top];
            deleted = [dmu;smu];
            animation = MAnimation.create [anim_src @ movement_animation_list;anim_dst]
          }
        end
        (*if the entity isn't dead*)
      else
        let new_dmu = dmu#remove_hp damage
        in 
        let () =
          MGrid.remove_at grid dr dq dst_layer;
          MGrid.set_at grid dr dq new_dmu dst_layer;
        in
        {
          added = [smu_without_mp;new_dmu];
          deleted = [dmu;smu];
          animation = MAnimation.create [anim_src;anim_dst]
        }

  (* Refill a unit movement point *)
  let refill_mp grid src layer =
    let sr,sq = MHex.get_r src,MHex.get_q src
    in
    let mu = MGrid.get_at grid sr sq layer in
    let new_mu = mu#refill_mp in
    let () =
      MGrid.remove_at grid sr sq layer;
      MGrid.set_at grid sr sq new_mu layer;
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
  let move grid src dst layer =
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
      let ent = MGrid.get_at grid sr sq layer in
      let old_ent,new_ent = 
        ent,ent#move dr dq
      in

      let start = MGrid.get_tile old_ent#get_r old_ent#get_q grid in
      let goal = MGrid.get_tile new_ent#get_r new_ent#get_q grid in
      let path_taken,mv_cost = MPathfinder.dijkstra_path start goal grid old_ent#get_current_mp layer in
      let movement_animation_list =
        List.fold_left (
          fun acc x -> 
            let new_ent = (old_ent#move x#get_r x#get_q)#set_status MEntity.MOVING in
            let au = MAnimation.create_animation_unit new_ent (None) 10 in
            au :: acc
        ) [] (List.rev path_taken)
      in
      let new_ent_minus_current_mp = new_ent#remove_mp mv_cost in
      let () =
        MGrid.remove_at grid sr sq layer;
        MGrid.set_at grid dr dq new_ent_minus_current_mp layer;
      in
      {
        added = [new_ent_minus_current_mp];
        deleted = [old_ent];
        animation = MAnimation.create [movement_animation_list]
      }

  (* Simply passes a turn, ie removing the remaining movement point *)
  let pass grid src layer =
    let sr = MHex.get_r src in
    let sq = MHex.get_q src in
    let ent = MGrid.get_at grid sr sq layer in
    let old_ent,new_ent =
      ent,ent#empty_mp
    in
    let () =
      MGrid.remove_at grid sr sq layer;
      MGrid.set_at grid sr sq new_ent layer
    in
    {
      added = [new_ent];
      deleted = [old_ent];
      animation = MAnimation.create [] 
    }

  exception Only_city_can_spawn
  (* Spawn a soldier with the same faction as src *)
  let spawn grid src dst entity_enum =
    let city_src = MGrid.get_at_ax grid src MLayer_enum.INFRASTRUCTURE in
    (* Check if it is a city *)
    if not (city_src#check_unit_type MEntity_enum.CITY )then
      raise Only_city_can_spawn
    else
      (* Create a new soldier *)
      let new_entity = 
        match entity_enum with
        | MEntity_enum.SOLDIER ->
          let tmp = MMilitary.create_soldier (MHex.get_r dst) (MHex.get_q dst) city_src#get_faction  in
          tmp#empty_mp
        | _ -> raise Not_yet_implemented
      in
      (* Add him to the grid *)
      let () =
        MGrid.add_at grid new_entity
      in
      let anim_soldier =
        (* The soldier will move from the city to the next tile *)
        let first = (new_entity#move city_src#get_r city_src#get_q)#set_status MEntity.MOVING in
        let second = first#move new_entity#get_r new_entity#get_q in
        [
          MAnimation.create_animation_unit first None 10;
          MAnimation.create_animation_unit second None 10
        ]
      in
      (* Compute the new city *)
      let new_city = city_src#empty_mp in
      (* Removes it from the grid and add the new one *)
      let () =
        MGrid.remove_at grid (MHex.get_r src) (MHex.get_q src) city_src#get_lt;
        MGrid.add_at grid new_city
      in
      (* The city will disappear while the unit is moving, thus we will create an animation for the city too *)
      let anim_city = List.init 2 (fun i -> MAnimation.create_animation_unit new_city None 10 ) in
      {
        added = [new_entity;new_city];
        deleted = [city_src];
        animation = MAnimation.create [anim_soldier;anim_city]
      }

  (* This function allows to change a unit behaviour *)
  let change_behaviour grid src layer new_behaviour =
    let sr = MHex.get_r src in
    let sq = MHex.get_q src in
    let ent = MGrid.get_at grid sr sq layer in
    let old_ent,new_ent =
      ent,ent#set_behaviour new_behaviour
    in
    let () =
      MGrid.remove_at grid sr sq layer;
      MGrid.set_at grid sr sq new_ent layer
    in
    {
      added = [new_ent];
      deleted = [old_ent];
      animation = MAnimation.create []
    }

  (* One entity uses the healthpack on itself *)
  let self_use_healthpack grid amount entity inventory = 
    (* Fetch the coords of the entity *)
    let axial = entity#get_axial in
    let r = MHex.get_r axial in
    let q = MHex.get_q axial in

    (* Compute the new state *)
    let old_ent,new_ent =
      entity,((entity#set_inventory inventory)#empty_mp)#add_hp_max amount
    in

    (* Update the grid *)
    let () =
      MGrid.remove_at grid r q old_ent#get_lt;
      MGrid.add_at grid new_ent;
    in

    {
      added = [new_ent];
      deleted = [old_ent];
      animation = MAnimation.create [[MAnimation.create_animation_unit new_ent (Some (MFx.create_from_entity new_ent MFx.HEALING)) 70 ]]
    }
  exception Source_entity_doesnt_have_the_item
  (* One entity use a healthpack to another, they must be on the same layer,
     we also need to specify which item is used*)
  let use_healthpack grid item_code src dst layer =

    (* Fetch the source entity *)
    let sr = MHex.get_r src in
    let sq = MHex.get_q src in
    let src_ent = MGrid.get_at grid sr sq layer in

    (* Check if the src ent has the item *)
    let healthpack,new_inventory = MInventory.fetch_item src_ent#get_inventory item_code in
    match healthpack with
    | None -> raise Source_entity_doesnt_have_the_item
    | Some healthpack ->
      let amount = MItem.get_amount_of_healthpack healthpack in
      if dst = src then
        self_use_healthpack grid amount src_ent new_inventory
      else
        (* Fetch the destination entity *)
        let dr = MHex.get_r dst in
        let dq = MHex.get_q dst in
        let dst_ent = MGrid.get_at grid dr dq layer in


        (* Update the source entity *)
        let old_src_ent,new_src_ent =
          src_ent,(src_ent#set_inventory new_inventory)#empty_mp
        in
        let () =
          MGrid.remove_at grid sr sq layer;
          MGrid.set_at grid sr sq new_src_ent layer;
        in

        (* Update the destination entity *)
        let old_dst_ent,new_dst_ent =
          dst_ent,(src_ent#add_hp_max (MItem.get_amount_of_healthpack healthpack))
        in
        let () =
          MGrid.remove_at grid dr dq layer;
          MGrid.set_at grid dr dq new_dst_ent layer;
        in

        {
          deleted = [old_dst_ent;old_src_ent];
          added = [new_dst_ent;new_src_ent];

          animation = MAnimation.create [[MAnimation.create_animation_unit new_src_ent (Some (MFx.create_from_entity new_src_ent MFx.HEALING)) 70 ]]
        }

  let use_nuke grid item_code src dst layer =
    (* Fetch the source entity *)
    let sr = MHex.get_r src in
    let sq = MHex.get_q src in
    let src_ent = MGrid.get_at grid sr sq layer in

    (* Check if the src ent has the item *)
    let nuke,new_inventory = MInventory.fetch_item src_ent#get_inventory item_code in
    match nuke with
    | None -> raise Source_entity_doesnt_have_the_item
    | Some nuke ->
      let radius = MItem.get_radius_of_nuke nuke in

      (* Fetch the dst tile *)
      let center = MGrid.get_tile_ax dst grid in
      let zone = center :: MGrid.range_tile grid center radius in

      (* Compute the new source unit that doesn't have the nuke anymore *)
      let new_src_ent = (src_ent#set_inventory new_inventory)#empty_mp in

      (* Compute the added units *)
      let added,animation_of_src =
        MGrid.remove_at grid (MHex.get_r src) (MHex.get_q src) layer;
        if not (List.exists ( fun x -> x#get_axial = src_ent#get_axial ) zone) then
          let () =
            MGrid.add_at grid new_src_ent
          in
          [new_src_ent],[MAnimation.create_animation_unit new_src_ent None 70]
        else
          [],[]
      in

      (*Compute the deleted units*)
      (* TODO : this is not optimal change *)
      let aux layer = 
        List.fold_left (
          fun (deleted,animation) tile ->
            try
              let entity = MGrid.get_at_ax grid tile#get_axial layer in
              (* Update the grid *)
              let () =
                MGrid.remove_at grid entity#get_r entity#get_q entity#get_lt;
              in
              let anim_unit = MAnimation.create_animation_unit entity (Some (MFx.create_from_entity entity MFx.ATTACKED)) 70 in
              (entity :: deleted),([anim_unit] :: animation)
            with 
              MGrid.Grid_cell_no_entity 
            | Invalid_argument _ -> (deleted,animation)
        ) ([],[]) zone
      in
      let deleted_military,animation_military = aux MLayer_enum.MILITARY
      in
      let deleted_infrastructure,animation_infrastructure = aux MLayer_enum.INFRASTRUCTURE
      in

    let animation = MAnimation.create_nuke_drop src_ent dst (deleted_infrastructure @ deleted_military) 40 15 70 in

      {
        deleted = src_ent :: (deleted_military @ deleted_infrastructure);
        added = added;
        (* animation = MAnimation.create (animation_of_src :: animation_military @ animation_infrastructure) *)
        animation = animation
      }


  exception Item_code_and_param_dont_match
  (* This function will direct to the right function for which item it is used *)
  let use_item grid item param = 
    match item,param with
    | MItem.HEALTHPACK _ ,MItem.HEALTHPACK_P(src,dst,layer) -> use_healthpack grid item src dst layer
    | MItem.NUKE _ ,MItem.NUKE_P(src,dst,layer) -> use_nuke grid item src dst layer
    | _ -> raise Not_yet_implemented

  exception Entity_is_not_next_to_item
  let pickup_item grid src dst layer =
    (* Fetch the entity *)
    let ent = MGrid.get_at_ax grid src layer in
    (* Fetch the item *)
    let item = MGrid.get_item_at_ax grid dst in

    (* Verify if src can pick up the item *)
    let tile_src = MGrid.get_tile_ax ent#get_axial grid in
    let zone = tile_src :: MGrid.range_tile grid tile_src 1 in

    if not (List.exists (fun x -> x#get_axial = item#get_axial) zone ) then 
      raise Entity_is_not_next_to_item
    else
      (* Modify the grid *)
      let () =
        (* Remove the old state of the grid *)
        MGrid.remove_item_at grid (MHex.get_r dst) (MHex.get_q dst);
        MGrid.remove_at grid (MHex.get_r src) (MHex.get_q src) layer;
      in

      (* Compute the new state *)
      let new_item = item#set_owned true in
      let new_ent = ent#add_item_to_inventory new_item in

      (* Modify the grid and apply the new state *)
      let () =
        MGrid.add_at grid new_ent;
      in
      {
        added = [new_ent];
        deleted = [ent];
        animation = MAnimation.create []
      }

  exception No_action_specified

  let execute t grid =
    match t with
    | None -> raise No_action_specified
    | Some e ->
      match e with
      | MAction_enum.MOVE (src,dst,layer) -> move grid src dst layer 
      | MAction_enum.ATTACK (src,dst,src_layer,dst_layer) -> attack grid src dst src_layer dst_layer
      | MAction_enum.REFILL_MP (src,layer) -> refill_mp grid src layer 
      | MAction_enum.PASS (src,layer) -> pass grid src layer
      | MAction_enum.SPAWN_ENTITY (src,dst,entity_enum) -> spawn grid src dst entity_enum
      | MAction_enum.USE_ITEM (item,param) -> use_item grid item param
      | MAction_enum.PICKUP_ITEM (src,dst,layer) -> pickup_item grid src dst layer
      | _ -> raise Not_yet_implemented
end
;;