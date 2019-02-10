open Tsdl
open Texture_wrapper
open Tile
open Utils
open Hex

(* Some functions are Heavily inspired by https://www.redblobgames.com/grids/hexagons/#coordinates as of 31/01/2019 *)
(* Constants *)
module MGrid = struct
    let () =  Random.self_init ()

    exception InvalidMap

    type t = {
        level_radius : int;
        grid : (MTile.tile) array array
    }

    let create_grid level_radius =
        let size = level_radius * 2 + 1 in
        Array.init size ( fun r ->
            Array.init size ( fun q ->
                let n = Random.int 5 in
                let m = Random.int 7 in
                let tmp1 = level_radius - r in
                let tmp2 = q - tmp1 in

                if tmp2 < 0 || q >= size + tmp1 then
                    new MTile.tile r q (MTile.int_to_tile_type n) (MTile.LAKE)
                else
                    new MTile.tile r q (MTile.int_to_tile_type n) (MTile.int_to_terrain_feature m)
            )
        )

    let get_tile r q t =
        let t = t.grid in
        try
            t.(r).(q)
        with Invalid_argument e ->
            new MTile.tile r q (MTile.TILE_GRASSLAND) (MTile.LAKE)

    let get_tile_cube cu t =
        let ax = MHex.cube_to_axial cu in
        get_tile ax.r ax.q t

    let create level_radius = 
    {
        level_radius = level_radius;
        grid = create_grid level_radius
    }

    let render renderer tile_texture terrain_feature_texture grid camera = 
        Array.iter (fun x ->
            Array.iter (fun y ->
                MTile.render renderer y tile_texture terrain_feature_texture camera
            ) x
        ) grid.grid

    type neighbours_t = {
        right : MTile.tile;
        top_right : MTile.tile;
        top_left : MTile.tile;
        left : MTile.tile;
        bot_right : MTile.tile;
        bot_left : MTile.tile;
    }

    let neighbours tile grid =
        let hex_neighbours = MHex.neighbours tile#get_cube in
        {
            right = get_tile_cube hex_neighbours.right grid;
            top_right = get_tile_cube hex_neighbours.top_right grid;
            top_left = get_tile_cube hex_neighbours.top_left grid;
            left = get_tile_cube hex_neighbours.left grid;
            bot_right = get_tile_cube hex_neighbours.bot_right grid;
            bot_left = get_tile_cube hex_neighbours.bot_left grid;
        }

    let neighbours_to_list neighbours =
        [
            neighbours.right;
            neighbours.top_right;
            neighbours.top_left;
            neighbours.left;
            neighbours.bot_left;
            neighbours.bot_right
        ]

    let neighbours_list tile grid = 
        let tmp = neighbours tile grid in
        neighbours_to_list tmp

   
        (* let dikstra start goal grid =
            let frontier = MPriority_queue.insert MPriority_queue.empty 0 start in
            let came_from = Hashtbl.create 23 in
            let cost_so_far = Hashtbl.create 23 in *)

end
;;