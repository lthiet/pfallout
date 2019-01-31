open Tsdl
open Texture_wrapper
open Binder
open Tile
open Utils

(* Constants *)
module MGrid = struct
    let level_width = 18
    let level_height = 14
    let tiles : MTile.tile list ref = ref []
    let hex_tiles : MTile.tile array array ref = ref [||]

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
            if !x >= level_width then (
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
            Array.init level_width ( fun q ->
                Array.init level_height ( fun r ->
                    let n = Random.int 5 in
                    new MTile.tile 0 q r (MTile.int_to_tile_type n)
                )
            )

        let init () =
            (* Load the map *)
            let map = open_in "asset/map/lazy.map" in
            (* Load the tiles *)
            let tmp = List.rev (map_into_char_list map []) in
            tiles := char_list_into_tile_list tmp;
            (* Close the flux *)
            close_in map;
            hex_tiles := create_map ();
end

module GridGraphics = struct
    let init renderer =
        Random.self_init ();
        TileGraphics.init renderer;
        MGrid.init ()

       

    let render renderer camera = 
        Array.iter (fun x ->
            Array.iter (fun y ->
                TileGraphics.render renderer y camera
            ) x
        ) !(MGrid.hex_tiles)
end
;;