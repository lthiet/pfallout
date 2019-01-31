open Tsdl
open Binder
open Game_object
open Hex
open Texture_wrapper
open Utils

module MTile = struct
    exception UnknownTiletype

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

    class tile id q r tile_type =
    object(self)
        inherit game_object id q r as super
        val tile_type : tile_type = tile_type
        method get_tile_type = tile_type
    end
end
;;

module TileGraphics = struct
    (* Modules variables *)
    let texture_path = [|"asset/image/tiles.png"|]
    let textures = ref [||]


    (* Functions *)
    let get_screen_x t = let x,_ = HexGraphics.axial_to_screen_coord t#get_axial in x 
    let get_screen_y t = let _,y = HexGraphics.axial_to_screen_coord t#get_axial in y
    let get_box t = Sdl.Rect.create (get_screen_x t) (get_screen_y t) HexGraphics.width HexGraphics.height

    (* Match a tile type to a clip to get the texture from *)
    let match_tile_type_to_clip t =
        let tw,th = HexGraphics.width,HexGraphics.height in
        let x,y,w,h = match t with
        | MTile.TILE_GRASSLAND -> 0, 0, tw,th 
        | MTile.TILE_DESERT -> 138, 0,  tw,th
        | MTile.TILE_SNOW -> 276, 0,  tw,th
        in
        Sdl.Rect.create x y w h

    (* Initializes the module *)
    let init renderer =
        (* Load the textures for tiles *)
        textures := MTexture.load_textures renderer texture_path

    (* Render a tile *)
    let render renderer tile camera =
        if check_collision (get_box tile) camera then
            MTexture.render renderer
            ~clip:( Some (match_tile_type_to_clip tile#get_tile_type))
            ~x:((get_screen_x tile)- Sdl.Rect.x camera)
            ~y:((get_screen_y tile)- Sdl.Rect.y camera)
            (!textures).(0)
        else
            ()
end
;;