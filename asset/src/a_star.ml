open Hex
open Tile
open Grid

module MA_star = struct
    let breadth_first_search (center : MTile.tile) (grid : MGrid.t) = 
        let frontier = Queue.create () in
        Queue.add center frontier;
        let visited = Hashtbl.create 23 in
        Hashtbl.add visited center true;

        let rec aux frontier =
            try
                let current = Queue.take frontier in
                let nb = MGrid.neighbours_to_list (MGrid.neighbours current grid) in
                List.iter ( fun x ->
                    try
                        let _ = Hashtbl.find visited x in
                        ()
                    with Not_found ->
                        Queue.add x frontier;
                        Hashtbl.add visited x true;
                ) nb;

            with Queue.Empty ->
                ()
        in
        aux frontier
end
;;