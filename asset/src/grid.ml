open Tsdl
open Texture_wrapper
open Texture_pack
open Tile
open Entity
open Military
open Tile
open Utils
open Hex
open Faction_enum
open Item
open Layer_enum

(* Some functions are Heavily inspired by https://www.redblobgames.com/grids/hexagons/#coordinates as of 31/01/2019 *)
(* The grid module is  where all the game object are stored. Currently
   there are 2 layers : 
   1 : tiles
   2 : military units

   In the future, there will be infrastructure layer. In short,
   each instance from different layer may be on top of each other,
   however two instances of same layer
   cannot be on the same coordinates *)
module MGrid = struct

  let () =  Random.self_init ()

  exception InvalidMap

  type t = {
    level_radius : int;
    tile_grid : (MTile.tile) array array;
    item_grid : (MItem.t option) array array;
    military_grid : (MEntity.t option) array array;
    infrastructure_grid : (MEntity.t option) array array;
  }

  let get_tile_grid t = t.tile_grid
  let get_item_grid t = t.item_grid

  let empty_item_at t r q =
    let g = get_item_grid t in
    match g.(r).(q) with
    | None -> true
    | _ -> false

  let remove_item_at t r q =
    let g = get_item_grid t in
    g.(r).(q) <- None


  exception No_item
  let get_item_at t r q =
    let g = get_item_grid t in
    match g.(r).(q) with
    | None -> raise No_item
    | Some x -> x

  let get_item_at_ax t ax =
    get_item_at t (MHex.get_r ax) (MHex.get_q ax)

  exception Item_grid_not_empty
  let set_item_at t r q item =
    let g = get_item_grid t in
    (* Check if there's already something *)
    match g.(r).(q) with
    (* If not, we can set *)
    | None -> g.(r).(q) <- Some item
    (* Otherwise, it is an error*)
    | _ -> 
      raise Item_grid_not_empty

  let add_item_at t item =
    set_item_at t item#get_r item#get_q item

  let get_military_grid t = t.military_grid
  let get_infrastructure_grid t = t.infrastructure_grid

  let get_grid_layer t layer =
    match layer with
    | MLayer_enum.MILITARY -> get_military_grid t
    | MLayer_enum.INFRASTRUCTURE -> get_infrastructure_grid t

  let empty_at t r q layer =
    let g = get_grid_layer t layer in
    match g.(r).(q) with
    | None -> true
    | _ -> false

  let remove_at t r q layer =
    let g = get_grid_layer t layer in
    g.(r).(q) <- None

  exception Grid_cell_no_entity
  let get_at t r q layer =
    let g = get_grid_layer t layer in
    match g.(r).(q) with
    | None -> raise Grid_cell_no_entity
    | Some x -> x

  let get_at_ax t ax layer =
    get_at t (MHex.get_r ax) (MHex.get_q ax) layer

  exception Grid_cell_not_empty
  exception Grid_set_error 
  let set_at t r q e layer =
    let g = get_grid_layer t layer in
    if e#check_layer layer then
      (* Check if there's already something *)
      match g.(r).(q) with
      (* If not, we can set *)
      | None -> g.(r).(q) <- Some e
      (* Otherwise, it is an error*)
      | _ -> 
        raise Grid_cell_not_empty
    else 
      raise Grid_set_error

  let add_at t e =
    set_at t e#get_r e#get_q e e#get_lt

  let create_grid level_radius =
    let size = level_radius * 2 + 1 in
    Array.init size ( fun r ->
        Array.init size ( fun q ->
            let n = Random.int 5 in
            let m = Random.int 10 in
            let tmp1 = level_radius - r in
            let tmp2 = q - tmp1 in

            if tmp2 < 0 || q >= size + tmp1 then
              new MTile.tile r q (MTile.int_to_tile_type n) (MTile.LAKE)
            else
              new MTile.tile r q (MTile.int_to_tile_type n) (MTile.int_to_terrain_feature m)
          )
      )

  let create_none_grid level_radius =
    let size = level_radius * 2 + 1 in
    Array.init size ( fun r ->
        Array.init size ( fun q ->
            None
          )
      )

  let get_tile r q t =
    let t = t.tile_grid in
    try
      t.(r).(q)
    with Invalid_argument e ->
      new MTile.tile r q (MTile.TILE_GRASSLAND) (MTile.LAKE)

  let get_tile_ax ax t =
    let r = MHex.get_r ax in
    let q = MHex.get_q ax in
    get_tile r q t

  let get_tile_cube cu t =
    let ax = MHex.cube_to_axial cu in
    get_tile ax.r ax.q t

  let create level_radius = 
    {
      level_radius = level_radius;
      tile_grid = create_grid level_radius;
      item_grid = create_none_grid level_radius;
      military_grid = create_none_grid level_radius;
      infrastructure_grid = create_none_grid level_radius;
    }

  let render renderer txt_pack grid scale camera frame_n = 

    let tile_texture =
      MTexture_pack.get_tile txt_pack
    in
    let terrain_feature_texture =
      MTexture_pack.get_terrain_feature txt_pack
    in
    Array.iter (fun x ->
        Array.iter (fun y ->
            MTile.render renderer y tile_texture terrain_feature_texture scale camera
          ) x
      ) grid.tile_grid;
    Array.iter (fun x ->
        Array.iter (fun y ->
            match y with
            | Some e ->
              MItem.render renderer e txt_pack scale camera frame_n
            | None -> ()
          ) x
      ) grid.item_grid;


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

  let range_tile grid t n =
    let l = MHex.range_ax n t#get_axial in
    List.fold_left (fun acc x -> 
        let tmp = get_tile_cube x grid in
        if t <> tmp then
          tmp :: acc
        else
          acc
      ) [] l

  let get_itemless_tile t
      ?(center : MHex.axial_coord = MHex.create_ax t.level_radius t.level_radius)
      ?(bound : int = t.level_radius )
      ()
    =
    let g = t.tile_grid in
    let r_r () = (MHex.get_r center) - bound + (Random.int (bound*2)) in
    let r_q () = (MHex.get_q center) - bound + (Random.int (bound*2)) in

    (* Careful, this function might not stop *)
    let rec aux r q =
      let res = g.(r).(q) in
      if res#is_impassable || not (empty_item_at t r q) then
        aux (r_r ()) (r_q ())
      else
        begin
          res
        end
    in aux (r_r ()) (r_q ())



  let get_random_accessible_tile t layer
      ?(center : MHex.axial_coord = MHex.create_ax t.level_radius t.level_radius)
      ?(bound : int = t.level_radius )
      ()
    =
    let g = t.tile_grid in
    let r_r () = (MHex.get_r center) - bound + (Random.int (bound*2)) in
    let r_q () = (MHex.get_q center) - bound + (Random.int (bound*2)) in

    (* Careful, this function might not stop *)
    let rec aux r q =
      let res = g.(r).(q) in
      if res#is_impassable || not (empty_at t r q layer) then
        aux (r_r ()) (r_q ())
      else
        begin
          res
        end
    in aux (r_r ()) (r_q ())

  let passable_tile_list list =
    List.fold_left (
      fun acc x ->
        if x#is_impassable then
          acc
        else
          x :: acc
    ) [] list

  (* Checks the whole list, if there's an unit, omit it*)
  let free_tile_list grid layer list = 
    List.fold_left (
      fun acc x -> 
        if empty_at grid x#get_r x#get_q layer then
          x :: acc
        else
          acc
    ) [] list

  let empty_tiles grid layer =
    let rec aux i acc =
      let rec aux_bis j acc_bis =
        if j < 0 then
          acc_bis
        else
          let new_acc = 
            let tile = get_tile i j grid in
            if empty_at grid i j layer && (not tile#is_impassable) then
              tile :: acc_bis
            else
              acc_bis
          in
          aux_bis (j-1) new_acc
      in
      if i < 0 then
        acc
      else
        aux (i-1) (aux_bis (grid.level_radius*2) acc)
    in
    aux (grid.level_radius*2) []

  (* Returns a lit of nearby enemies *)
  let nearby_enemies grid entity n layer =
    let tile = get_tile entity#get_r entity#get_q grid in
    let nearby_tiles = range_tile grid tile n in
    let rec aux l acc = 
      match l with
      | [] -> acc
      | x :: s ->
        try
          let entity_on_tile = get_at_ax grid x#get_axial layer in
          if entity#get_faction <> entity_on_tile#get_faction then
            aux s (entity_on_tile::acc)
          else
            aux s acc
        with 
        | Grid_cell_no_entity -> aux s acc
        | Invalid_argument _  -> aux s acc
    in aux nearby_tiles []

  (* Look for an item in the grid around a radius of range *)
  let nearby_item_of_type grid src range item_enum =
    let tile = get_tile_ax src grid in
    let zone = range_tile grid tile range in
    try
      let tmp = List.find
          ( fun x -> 
              try
                let found_item = get_item_at_ax grid x#get_axial in
                MItem.same_code_and_enum found_item#get_code item_enum
              with
                No_item | Invalid_argument _ -> false
          ) zone
      in Some tmp
    with Not_found -> None

end
;;