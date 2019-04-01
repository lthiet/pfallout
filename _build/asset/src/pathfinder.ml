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
      cost : int;
      came_from : ('a node) option
    }

  let rec next_move (start:MTile.t) (current:MTile.t node) =
    match current.came_from with
    | None -> Some (current.elem)
    | Some x when x.elem = start -> Some(current.elem)
    | Some x -> next_move start x

  (* Returns the next tile to get closer to the goal *)
  let a_star (start:MTile.t) (goal:MTile.t) grid layer =
    let frontier = 
      let tmp = MPriority_queue.empty
      in
      MPriority_queue.insert tmp 0 {
        elem = start;
        cost = 0;
        came_from = None
      }
    in

    let rec aux frontier already_seen =
      if MPriority_queue.is_empty frontier then
        None
      else
        let _,current,frontier = MPriority_queue.extract frontier in
        if current.elem = goal then 
          next_move start current
        else
          let already_seen = current.elem :: already_seen in
          let succ = MGrid.neighbours_list current.elem grid in
          let truc = List.fold_left (
              fun acc s -> 
                let s_node = {
                  elem = s;
                  cost = current.cost;
                  came_from = Some current
                }
                in
                if (List.exists ( fun x -> x = s) already_seen) || (MPriority_queue.contains s_node frontier) then
                  acc
                else
                  acc
            ) [] succ
          in
          None
    in
    aux frontier []



end
;;