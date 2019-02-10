open Tsdl
open Game_object
open Hex
open Texture_wrapper
open Utils

module MTile = struct
    exception UnknownTiletype

    type terrain_feature =
        | MOUNTAIN
        | HILL
        | FOREST
        | LAKE
        | REGULAR

    let terrain_feature_to_movement_cost tf =
        match tf with
        | MOUNTAIN -> -1
        | HILL -> 2
        | FOREST -> 2
        | LAKE -> -1
        | _ -> 1

    let int_to_terrain_feature n =
        match n with
        | 0 -> MOUNTAIN
        | 1 | 2 -> HILL
        | 3 | 4 -> FOREST
        | 5 -> LAKE
        | _ -> REGULAR

    let terrain_feature_to_int tf =
        match tf with
        | MOUNTAIN -> 0
        | FOREST -> 1
        | HILL -> 2
        | REGULAR -> 3
        | LAKE -> 4

    type tile_type =
        | TILE_GRASSLAND
        | TILE_DESERT 
        | TILE_SNOW 

    let tile_type_to_int t =
        match t with
        | TILE_GRASSLAND -> 0
        | TILE_DESERT  -> 1
        | TILE_SNOW  -> 2

    let int_to_tile_type n =
        match n with
        | 1 -> TILE_DESERT
        | 2  -> TILE_SNOW
        | 0 | _ -> TILE_GRASSLAND

    class tile r q tt tf =
    object(self)
        inherit game_object r q as super
        val tile_type : tile_type = tt
        val terrain_feature : terrain_feature = tf
        method get_tile_type = tile_type
        method get_terrain_feature = terrain_feature
        method is_mountain = terrain_feature = MOUNTAIN
        method is_lake = terrain_feature = LAKE
        method is_hill = terrain_feature = HILL
        method is_forest = terrain_feature = FOREST
        method is_regular = terrain_feature = REGULAR
        method get_movement_cost = terrain_feature_to_movement_cost terrain_feature
        method is_impassable = terrain_feature = MOUNTAIN || terrain_feature = LAKE
    end


    (* Match a tile type to a clip to get the texture from *)
    let match_tile_type_to_clip t =
        let tw,th = MHex.width,MHex.height in
        let x,y,w,h = match t with
        | TILE_GRASSLAND -> 0, 0, tw,th 
        | TILE_DESERT -> 138, 0,  tw,th
        | TILE_SNOW -> 276, 0,  tw,th
        in
        Sdl.Rect.create x y w h

    let match_terrain_feature_to_clip t =
        let tw,th = MHex.width,MHex.height in
        let i = terrain_feature_to_int t in
        Sdl.Rect.create (i*tw) 0 tw th

    (* Render a tile *)
    let render renderer tile tile_texture terrain_feature_texture camera =
        if check_collision tile#get_box camera && (not tile#is_lake) then
            let x,y = 
                let tmp1,tmp2 = MHex.axial_to_screen_coord tile#get_axial in
                tmp1 - Sdl.Rect.x camera,tmp2 - Sdl.Rect.y camera
            in
            MTexture.render renderer
            ~clip:( Some (match_tile_type_to_clip tile#get_tile_type))
            ~x:x
            ~y:y
            tile_texture;
            MTexture.render renderer
            ~clip:(Some (match_terrain_feature_to_clip tile#get_terrain_feature))
            ~x:x
            ~y:y
            terrain_feature_texture
        else
            ()
end
;;