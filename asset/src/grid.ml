open Tsdl
open Texture_wrapper
open Tile
open Utils

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
                let tmp1 = level_radius - r in
                let tmp2 = q - tmp1 in

                if tmp2 < 0 || q >= size + tmp1 then
                    None
                else
                    Some (new MTile.tile 0 r q (MTile.int_to_tile_type n))
            )
        )

    let get_tile r q t =
        let t = t.grid in
        try
            t.(r).(q)
        with Invalid_argument e ->
            None

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
end
;;