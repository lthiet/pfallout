open Grid
open Action_enum
open Pathfinder
open Hex
open Animation
open Entity
open Military

module MAction = struct
    exception Not_implemented

    exception Impossible_movement

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
            let mu = MGrid.get_mg_at grid sr sq in
            let old_mu,new_mu = 
                match mu with
                | None -> 
                    raise Exit
                | Some x ->
                    x,x#move dr dq
            in
                        
            let start = MGrid.get_tile old_mu#get_r old_mu#get_q grid in
            let goal = MGrid.get_tile new_mu#get_r new_mu#get_q grid in
            let path_taken,mv_cost = MPathfinder.dijkstra_path start goal grid old_mu#get_mp in
            let movement_animation_list =
                List.fold_left (
                    fun acc x -> (
                        MMilitary.military_to_entity (old_mu#move x#get_r x#get_q),10) :: acc
                ) [] (List.rev path_taken)
            in
            let new_mu_minus_mp = new_mu#remove_mp mv_cost in
            let () =
                MGrid.remove_mg_at grid sr sq;
                MGrid.set_mg_at grid dr dq new_mu_minus_mp;
            in
            grid,[new_mu_minus_mp],[old_mu],(MAnimation.create [movement_animation_list])

    let execute t grid src_ax dst_ax =
        match t with
        | MAction_enum.MOVE -> move grid src_ax dst_ax
        | _ -> raise Not_implemented
end
;;