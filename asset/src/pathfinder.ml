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
      state : 'a;
      g : int;
      f : int;
      father : ('a node) option
    }

  let rec next_move start current =
    match current.father with
    | None -> Some (current.state)
    | Some x when x.state = start -> Some(current.state)
    | Some x -> next_move start x

  let eligible_tile grid tile layer =
    let check1 = tile#is_impassable in
    let check2 =
      try 
        let _ = MGrid.get_at_ax grid tile#get_axial layer in false
      with MGrid.Grid_cell_no_entity | Invalid_argument _ -> true
    in
    check1 && check2

  let rec state_already_in set state =
    match set with
    | [] -> false
    | x :: s -> 
      if x.state = state then
        true
      else
        state_already_in s state

  let find_node_of_state_and_remove set state =
    let rec aux set state acc_set acc_node =
      match set with
      | [] -> 
        begin
          match acc_node with
          | None -> raise Not_found
          | Some x -> x,acc_set
        end
      | x :: s -> 
        if x.state = state then
          aux s state acc_set (Some x)
        else
          aux s state (x :: acc_set) acc_node
    in
    aux set state [] None


  (* Iterate through the frontier and poll the next state to check,
     return : the new frontier and the next state to compute*)
  let pollMin frontier =
    match frontier with
    | [] -> raise Empty_list
    | x :: s ->
      let rec aux frontier new_frontier min_state =
        match frontier with
        (* Look done, return the values *)
        | [] -> new_frontier,min_state
        | y :: r ->
          (* Check if the next state is more interesting, ie better cost *)
          let new_min_state,state_put_back =
            if y.f < min_state.f then
              y,min_state
            else
              min_state,y
          in
          aux r (state_put_back::new_frontier) new_min_state
      in
      aux s [] x

  (* Iterate through successors and add them to the frontier, remove them from already if we found a better path to them *)
  let add_successor_to_frontier goal grid layer frontier_without_current already_dev_with_current successors current = 
    let rec aux frontier already_dev acc_frontier acc_already_dev successors =
      match successors with
      | [] -> acc_frontier,acc_already_dev
      | x_succ :: s_succ ->
        (* Compute the new frontier and already dev *)
        let new_frontier,new_already_dev = 
          (* The successor is not already seen, create a new node *)
          if not (state_already_in frontier x_succ || state_already_in already_dev x_succ) then
            let node_succ = {
              state = x_succ;
              g = current.g + x_succ#get_movement_cost;
              f = MHex.dist_cube x_succ#get_cube goal#get_cube;
              father = Some current
            } in
            node_succ :: acc_frontier,acc_already_dev
            (* The succ is already seen, there might be a new  *)
          else
            acc_frontier,acc_already_dev
        in
        acc_frontier,acc_already_dev
    in
    aux frontier_without_current already_dev_with_current [] [] successors



  let rec a_star_loop (start:MTile.t) (goal:MTile.t) grid layer frontier already_dev = 
    (* No path found *)
    if (List.length frontier) <= 0 then
      None

    (* Frontier isn't empty*)
    else
      (* Poll the most interesting state *)
      let frontier_without_current,current = pollMin frontier in
      (* Add it to the already developed *)
      let already_dev_with_current = current :: already_dev in
      let successors = MGrid.neighbours_list current.state grid in
      (* Iterate through each succesor and update the frontier and already dev accordingly *)
      let _ = add_successor_to_frontier goal grid layer frontier_without_current already_dev_with_current successors current in
      None



  (* Returns the next tile to get closer to the goal *)
  let a_star (start:MTile.t) (goal:MTile.t) grid layer =
    (* Init *)
    (* The list of state already seen *)
    let already_dev = [] in
    (* The initial state *)
    let state_init = {
      state = start;
      g = 0;
      f = MHex.dist_cube start#get_cube goal#get_cube;
      father = None
    } in
    let frontier = [state_init] in
    a_star_loop start goal grid layer frontier already_dev


  let closest_tile src dst grid layer =
    match a_star src dst grid layer with
    | None -> raise No_path_found
    | Some x -> x


  (* Tests *)
  let () =
    let l = [
      {
        state = 1;
        g = 0;
        f = 4;
        father = None;
      };
      {
        state = 2;
        g = 0;
        f = 6;
        father = None;
      };
      {
        state = 3;
        g = 0;
        f = 1;
        father = None;
      };
    ] in
    let truc,bidule = pollMin l in
    print_int bidule.f;
    print_newline ();
    print_newline ();
    List.iter (fun x ->
        print_int x.f;print_newline();) truc;

    let toto,tata = find_node_of_state_and_remove l 3 in
    print_newline ();
    print_int toto.state;
    print_newline ();
    print_newline ();
    List.iter (fun x ->
        print_int x.state;print_newline();) tata;

end
;;