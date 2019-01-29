open Tsdl
open Binder
open Game_object
open Texture_wrapper
open Utils

module MTile = struct
    exception UnknownTiletype

    type tile_type =
        | TILE_RED
        | TILE_GREEN 
        | TILE_BLUE 
        | TILE_CENTER 
        | TILE_TOP 
        | TILE_TOPRIGHT 
        | TILE_RIGHT 
        | TILE_BOTTOMRIGHT 
        | TILE_BOTTOM 
        | TILE_BOTTOMLEFT 
        | TILE_LEFT 
        | TILE_TOPLEFT 

    let tile_type_to_int t =
        match t with
        | TILE_RED -> 0
        | TILE_GREEN  -> 1
        | TILE_BLUE  -> 2
        | TILE_CENTER  -> 3
        | TILE_TOP  -> 4
        | TILE_TOPRIGHT  -> 5
        | TILE_RIGHT  -> 6
        | TILE_BOTTOMRIGHT  -> 7
        | TILE_BOTTOM  -> 8
        | TILE_BOTTOMLEFT  -> 9
        | TILE_LEFT  -> 10
        | TILE_TOPLEFT  -> 11

    let int_to_tile_type n =
        match n with
        | 0 -> TILE_RED
        | 1 -> TILE_GREEN
        | 2 -> TILE_BLUE
        | 3 -> TILE_CENTER
        | 4 -> TILE_TOP
        | 5 -> TILE_TOPRIGHT
        | 6 -> TILE_RIGHT
        | 7 -> TILE_BOTTOMRIGHT
        | 8 -> TILE_BOTTOM
        | 9 -> TILE_BOTTOMLEFT
        | 10 -> TILE_LEFT
        | 11 -> TILE_TOPLEFT
        | _ -> TILE_RED

    let tile_height = 80
    let tile_width = tile_height

    class tile id x y z tile_type =
    object(self)
        val box = Sdl.Rect.create x y tile_height tile_width
        inherit game_object id x y z as super
        val tile_type : tile_type = tile_type
        val h = tile_height
        val w = tile_width
        method get_tile_type = tile_type
        method get_box = box
    end
end
;;

module TileGraphics = struct
    (* Modules variables *)
    let texture_path = [|"asset/image/tiles.png"|]
    let textures = ref [||]
    let clips = [||]
    let tiles : MTile.tile list ref = ref []
    let level_width = 1280
    let level_height = 960

    exception InvalidMap

    (* Functions *)

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
        List.fold_left ( fun acc (a,b) -> 
            let a_bis = 10 * (int_of_char a) in
            let b_bis = int_of_char b in
            let tile_type_n = a_bis + b_bis in
            let tile_type = MTile.int_to_tile_type tile_type_n in
            let tile = new MTile.tile 0 !x !y 0 tile_type in

            x := !x + MTile.tile_width;
            if !x >= level_width then (
                x := 0;
                y := !y + MTile.tile_height;
            )
            else
            (
                ();
            );
            tile :: acc
        ) [] l


    (* Match a tile type to a clip to get the texture from *)
    let match_tile_type_to_clip t =
        let x,y,w,h = match t with
        | MTile.TILE_RED -> 0, 0, MTile.tile_width, MTile.tile_height
        | MTile.TILE_GREEN -> 0, 80, MTile.tile_width, MTile.tile_height
        | MTile.TILE_BLUE -> 0, 160, MTile.tile_width, MTile.tile_height
        | MTile.TILE_TOPLEFT -> 80, 0, MTile.tile_width, MTile.tile_height
        | MTile.TILE_LEFT -> 80, 80, MTile.tile_width, MTile.tile_height
        | MTile.TILE_BOTTOMLEFT -> 80, 160, MTile.tile_width, MTile.tile_height
        | MTile.TILE_TOP -> 80, 0, MTile.tile_width, MTile.tile_height
        | MTile.TILE_CENTER -> 160, 80, MTile.tile_width, MTile.tile_height
        | MTile.TILE_BOTTOM -> 160, 160, MTile.tile_width, MTile.tile_height
        | MTile.TILE_TOPRIGHT -> 240, 0, MTile.tile_width, MTile.tile_height
        | MTile.TILE_RIGHT -> 240, 80, MTile.tile_width, MTile.tile_height
        | MTile.TILE_BOTTOMRIGHT -> 240, 160, MTile.tile_width, MTile.tile_height
        in
        Sdl.Rect.create x y w h

    (* Initializes the module *)
    let init renderer =
        (* Load the textures for tiles *)
        textures := MTexture.load_textures renderer texture_path;
        (* Load the map *)
        let map = open_in "asset/map/lazy.map" in
        (* Load the tiles *)
        let tmp = List.rev (map_into_char_list map []) in
        tiles := char_list_into_tile_list tmp;
        (* Close the flux *)
        close_in map

    (* Render a tile *)
    let render renderer tile camera =
        let clip_n = MTile.tile_type_to_int (tile#get_tile_type ()) in
        if check_collision tile#get_box camera then
            MTexture.render renderer
            ~clip:( Some (clips.(clip_n)))
            ~x:(tile#get_x)
            ~y:(tile#get_y)
            (!textures).(0)

end
;;