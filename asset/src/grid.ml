open Tsdl
open Texture_wrapper
open Binder
open Tile
open Utils

(* Constants *)
module MGrid = struct
    let level_radius = 2
    let hex_tiles : (MTile.tile option)array array ref = ref [||]

    exception InvalidMap

    (* Format the map file into workable data *)
    let rec map_into_char_list inc acc =
        try
            (* Read the input file *)
            let a = input_char inc in
            (* Check if a in a number *)
            if char_is_number a then
                (* Read the second letter *)
                let b = input_char inc in
                if char_is_number b then
                    (* Discard the space *)
                    let c = input_char inc in
                    if c = ' ' then
                        map_into_char_list inc ((a,b) :: acc)
                    else
                        raise InvalidMap
                else
                    raise InvalidMap
            else
                map_into_char_list inc acc
        with End_of_file ->
            acc

    (* Transform the workable data from map files into a tile list *)
    let char_list_into_tile_list l =
        let x = ref 0 in
        let y = ref 0 in
        let l =
        List.fold_left ( fun acc (a,b) -> 
            let a_bis = 10 * ((int_of_char a)-48) in
            let b_bis = (int_of_char b) - 48 in
            let tile_type_n = a_bis + b_bis in
            let tile_type = MTile.int_to_tile_type tile_type_n in
            let tile = new MTile.tile 0 !x !y tile_type in
            x := !x + 1;
            if !x >= level_radius then (
                x := 0;
                y := !y + 1;
            )
            else
            (
                ();
            );
            tile :: acc
        ) [] l
        in
        List.rev l

        let create_map () =
            let size = level_radius * 2 + 1 in
            Array.init size ( fun r ->
                Array.init size ( fun q ->
                    let n = Random.int 5 in
                    let tmp1 = level_radius - r in
                    let tmp2 = q - tmp1 in

                    if tmp2 < 0 || q >= size + tmp1 then
                        None
                    else
                        Some (new MTile.tile 0 r q (MTile.int_to_tile_type n))
                )
            )

        let init () =
            hex_tiles := create_map ()
end

module GridGraphics = struct
    let init renderer =
        Random.self_init ();
        TileGraphics.init renderer;
        MGrid.init ()

    let render renderer camera = 
        Array.iter (fun x ->
            Array.iter (fun y ->
                match y with
                | Some e ->
                    TileGraphics.render renderer e camera
                | None ->
                    ()
            ) x
        ) !(MGrid.hex_tiles)
end
;;