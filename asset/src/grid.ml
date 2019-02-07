open Tsdl
open Texture_wrapper
open Tile
open Utils
open Hex

(* Constants *)
module MGrid = struct
    let () =  Random.self_init ()

    exception InvalidMap

    type t = {
        level_radius : int;
        grid : (MTile.tile option) array array
    }

    let create_grid level_radius =
        let size = level_radius * 2 + 1 in
        Array.init size ( fun r ->
            Array.init size ( fun q ->
                let n = Random.int 5 in
                let m = Random.int 5 in
                let tmp1 = level_radius - r in
                let tmp2 = q - tmp1 in

                if tmp2 < 0 || q >= size + tmp1 then
                    None
                else
                    Some (new MTile.tile 0 r q (MTile.int_to_tile_type n) (MTile.int_to_terrain_feature m))
            )
        )

    let get_tile r q t =
        let t = t.grid in
        try
            t.(r).(q)
        with Invalid_argument e ->
            None

    let get_tile_cube cu t =
        let ax = MHex.cube_to_axial cu in
        get_tile ax.r ax.q t

    let create level_radius = 
    {
        level_radius = level_radius;
        grid = create_grid level_radius
    }

    let render renderer tile_texture grid camera = 
        Array.iter (fun x ->
            Array.iter (fun y ->
                match y with
                | Some e ->
                    MTile.render renderer e tile_texture camera
                | None ->
                    ()
            ) x
        ) grid.grid

    type neighbours_t = {
        right : MTile.tile option;
        top_right : MTile.tile option;
        top_left : MTile.tile option;
        left : MTile.tile option;
        bot_right : MTile.tile option;
        bot_left : MTile.tile option;
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
        let l1 = 
            match neighbours.right with
            | None ->
                []
            | Some t ->
                [t]
        in
        let l2 =
            match neighbours.top_right with
            | None ->
                l1
            | Some t ->
                t :: l1
        in
        let l3 =
            match neighbours.top_left with
            | None ->
                l2
            | Some t ->
                t :: l2
        in

        let l4 =
            match neighbours.left with
            | None ->
                l3
            | Some t ->
                t :: l3
        in

        let l5 =
            match neighbours.bot_left with
            | None ->
                l4
            | Some t ->
                t :: l4
        in

        match neighbours.bot_right with
        | None ->
            l5
        | Some t ->
            t :: l5
end
;;