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


    let a_star start goal grid =
        let frontier = Queue.create () in
        Queue.add start frontier;
        let came_from = Hashtbl.create 23 in

        (* Explore everything *)
        let rec aux1 grid =
            if not (Queue.is_empty frontier) then
            begin
                let current = Queue.take frontier in
                let n_l = MGrid.neighbours_list current grid in
                List.iter (
                    fun x ->
                        try
                            let _ = Hashtbl.find came_from x in
                            ()
                        with Not_found ->
                            if not x#is_impassable then
                            begin
                                Queue.add x frontier;
                                Hashtbl.add came_from x current
                            end
                ) n_l;
                aux1 grid
            end
        in
        aux1 grid;

        (* Retrace the path *)
        let rec aux2 current path =
            if current = start then
                path
            else
            begin
                let tmp = Hashtbl.find_opt came_from current in
                match tmp with
                | None ->
                    path
                | Some tmp ->
                    let b = MGrid.get_tile current#get_r current#get_q grid in
                    aux2 tmp (b::path)
            end
        in
        aux2 goal []

    let dijkstra start goal grid = 
        let frontier = 
            let tmp = MPriority_queue.empty
            in
            MPriority_queue.insert tmp 0 start
        in
        let come_from = Hashtbl.create 23 in
        let cost_so_far = Hashtbl.create 23 in
        Hashtbl.add cost_so_far start 0

        (* let rec aux frontier start goal grid =
            if  *)



end
;;