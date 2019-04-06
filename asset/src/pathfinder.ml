open Tile
open Hex
open Grid
open Utils
open Priority_queue

module MPathfinder = struct
  (* TODO : functionalize *)
  let reachable_tile center grid movement =
    let visited = Hashtbl.create 23 in
    Hashtbl.add visited center true;

    let fringe = Array.make (movement+1) [] in
    fringe.(0) <- [center];

    let rec aux n fringe =
      if n > movement then
        ()
      else
        begin
          fringe.(n) <- [];
          List.iter ( fun x ->
              let nb_l = MGrid.neighbours_list x grid in
              List.iter ( fun y ->
                  try
                    let _ = Hashtbl.find visited y in
                    ()
                  with Not_found ->
                    if not y#is_impassable then
                      begin
                        Hashtbl.add visited y true;
                        fringe.(n) <- y :: fringe.(n)
                      end
                ) nb_l;
            ) fringe.(n-1);
          aux (n+1) fringe
        end
    in
    aux 1 fringe;

    let visited_list = ref [] in
    Hashtbl.iter (
      fun x y ->
        visited_list := x :: !visited_list
    ) visited;
    !visited_list

  exception No_path_found
  let trace_path come_from start goal grid =  
    (* Retrace the path *)
    let rec aux current path cost =
      if current = start then
        path,cost
      else
        begin
          let tmp = Hashtbl.find_opt come_from current in
          match tmp with
          | None ->
            begin
              if current = start then
                begin
                  path,cost
                end
              else
                raise No_path_found
            end
          | Some tmp ->
            let b = MGrid.get_tile current#get_r current#get_q grid in
            aux tmp (b::path) (cost + b#get_movement_cost)
        end
    in
    aux goal [] 0

  let reachable_tile come_from =
    Hashtbl.fold (fun k e acc -> 
        k :: acc
      ) come_from []

  let dijkstra start goal grid all range layer = 
    let frontier = 
      let tmp = MPriority_queue.empty
      in
      MPriority_queue.insert tmp 0 start
    in
    let come_from = Hashtbl.create 23 in
    let cost_so_far = Hashtbl.create 23 in
    Hashtbl.add cost_so_far start 0;

    let rec aux frontier start goal grid =
      if not (MPriority_queue.is_empty frontier) then
        begin
          let _,current,new_frontier = MPriority_queue.extract frontier in
          if current != goal || all then
            begin
              let n_l = MGrid.neighbours_list current grid in
              let updated_frontier = List.fold_left (
                  fun acc x ->
                    if (not x#is_impassable) && (MGrid.empty_at grid x#get_r x#get_q layer || x = goal) then
                      begin
                        let new_cost = (Hashtbl.find cost_so_far current) + x#get_movement_cost in
                        let x_in_cost_so_far =
                          try
                            let _ = Hashtbl.find cost_so_far x in
                            true
                          with Not_found ->
                            false
                        in
                        if ((not x_in_cost_so_far) || new_cost < (Hashtbl.find cost_so_far x)) && new_cost <= range then
                          begin
                            Hashtbl.add come_from x current;
                            Hashtbl.remove cost_so_far x;
                            Hashtbl.add cost_so_far x new_cost;
                            let priority = new_cost in
                            MPriority_queue.insert acc priority x
                          end
                        else
                          begin
                            acc
                          end
                      end
                    else
                      begin
                        acc
                      end
                ) new_frontier n_l
              in
              aux updated_frontier start goal grid
            end
        end
    in
    aux frontier start goal grid;
    come_from

  let dijkstra_path start goal grid range layer = 
    let tmp = dijkstra start goal grid false range layer in
    let res1,res2 = trace_path tmp start goal grid in
    (start :: res1),res2

  let dijkstra_reachable start goal grid range layer = 
    let tmp = dijkstra start goal grid true range layer in
    reachable_tile tmp


  type 'a node =
    {
      elem : 'a;
      cost_so_far : int;
      came_from : ('a node) option
    }

  let rec next_move (start:MTile.t) (current:MTile.t node) =
    match current.came_from with
    | None -> Some (current.elem)
    | Some x when x.elem = start -> Some(current.elem)
    | Some x -> next_move start x

  let eligible_tile grid tile layer =
    let check1 = tile#is_impassable in
    let check2 =
      try 
        let _ = MGrid.get_at_ax grid tile#get_axial layer in false
      with MGrid.Grid_cell_no_entity | Invalid_argument _ -> true
    in
    check1 && check2

  (* Returns the next tile to get closer to the goal *)
  let a_star (start:MTile.t) (goal:MTile.t) grid layer =
    (* Initialisation *)
    let frontier = 
      let tmp = MPriority_queue.empty
      in
      let elem : MTile.t node =  {
        elem = start;
        cost_so_far = 0;
        came_from = None
      } in
      MPriority_queue.insert tmp 0 elem
    in

    let rec aux frontier already_seen =
      (* If there are no more in the frontier, there are no path *)
      if MPriority_queue.is_empty frontier then
        None
      else
        (* Fetch the next tile to process *)
        let _,current,frontier_without_current = MPriority_queue.extract frontier in

        (* We found the goal, construct the path *)
        if current.elem = goal then 
          next_move start current
        else
          (* We will process for the current tile, add it to already seen *)
          let already_seen = current :: already_seen in
          (* Compute the neighbouring tiles *)
          let succ_s = MGrid.neighbours_list current.elem grid in

          (*We will iterate over the list of successors  *)
          let res_frontier,res_already_seen = List.fold_left (
              (* Each time we loop at a successor we update the frontier and already seen *)
              fun (frontier_acc,already_seen_acc) (s:MTile.t) -> 

                (* Get the successor if he's in alreadu seen *)
                let succ_from_already_seen = 
                  List.find_opt ( fun x -> x.elem = s ) already_seen
                in
                let succ_found_in_already_seen = 
                  match succ_from_already_seen with
                  | None -> false
                  | _ -> true
                in

                (* The successor is not in frontier AND not in already seen*)
                if not (succ_found_in_already_seen || (MPriority_queue.exists (fun x -> x.elem = s) frontier_acc)) then
                  (* The successor cannot be accessed or there is someone *)
                  if not (eligible_tile grid s layer) then
                    (* Don't update the frontier or already seen *)
                    frontier_acc,already_seen_acc
                  else
                    (* The successor is a potential candidate, we compute its values *)
                    let new_best : MTile.t node = {
                      elem = s;
                      cost_so_far = current.cost_so_far + s#get_movement_cost;
                      came_from = Some current
                    } in
                    (* Cost so far + heuristic *)
                    let potential_cost = new_best.cost_so_far + (MHex.distance_cu s#get_cube goal#get_cube) in 
                    let new_frontier = MPriority_queue.insert frontier_acc potential_cost new_best
                    in
                    (* Return the updated frontier, already seen not updated*)
                    new_frontier,already_seen_acc
                else if succ_found_in_already_seen then
                  begin
                    let () = debug "truc" in
                    match succ_from_already_seen with
                    | None -> raise Exit
                    | Some succ -> 
                      let new_cost =  current.cost_so_far + succ.elem#get_movement_cost in
                      if (new_cost < succ.cost_so_far) then
                        let new_frontier,new_already_seen =
                          List.fold_left (
                            fun (new_frontier_acc,new_already_seen_acc) x ->
                              let f = ( fun alseen -> alseen.elem = x ) in
                              if List.exists f already_seen_acc then
                                let new_already_seen_acc = remove_f new_already_seen_acc [] f in
                                let new_frontier_acc = MPriority_queue.insert new_frontier_acc (new_cost + MHex.dist_cube goal#get_cube x#get_cube ) {elem = x; cost_so_far = x#get_movement_cost + current.cost_so_far;came_from = Some current } in
                                new_frontier_acc,new_already_seen_acc
                              else
                                new_frontier_acc,new_already_seen_acc
                          ) (frontier,[]) succ_s
                        in
                        (* Return *)
                        new_frontier,new_already_seen
                      else  
                        (* Return *)
                        frontier_acc,already_seen_acc
                  end
                else
                  (* Return *)
                  frontier_acc,already_seen_acc
            ) (frontier_without_current,already_seen) succ_s
          in
          aux res_frontier res_already_seen
    in
    aux frontier []

  let closest_tile src dst grid layer =
    match a_star src dst grid layer with
    | None -> raise No_path_found
    | Some x -> x
end
;;