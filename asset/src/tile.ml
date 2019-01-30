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


    class tile id x y z tile_type =
    object(self)
        inherit game_object id x y z as super
        val tile_type : tile_type = tile_type
        method get_tile_type = tile_type
    end
end
;;

module TileGraphics = struct
    (* Modules variables *)
    let texture_path = [|"asset/image/tiles.png"|]
    let textures = ref [||]
    let tile_height = 80
    let tile_width = tile_height


    (* Functions *)
    let get_screen_x t = t#get_x * tile_width
    let get_screen_y t = t#get_y * tile_height
    let get_box t = Sdl.Rect.create (get_screen_x t) (get_screen_y t) tile_width tile_height

    (* Match a tile type to a clip to get the texture from *)
    let match_tile_type_to_clip t =
        let tw,th = tile_height,tile_height in
        let x,y,w,h = match t with
        | MTile.TILE_RED -> 0, 0, tw,th 
        | MTile.TILE_GREEN -> 0, 80,  tw,th
        | MTile.TILE_BLUE -> 0, 160,  tw,th
        | MTile.TILE_TOPLEFT -> 80, 0,  tw,th
        | MTile.TILE_LEFT -> 80, 80,  tw,th
        | MTile.TILE_BOTTOMLEFT -> 80, 160,  tw,th
        | MTile.TILE_TOP -> 160, 0,  tw,th
        | MTile.TILE_CENTER -> 160, 80,  tw,th
        | MTile.TILE_BOTTOM -> 160, 160,  tw,th
        | MTile.TILE_TOPRIGHT -> 240, 0,  tw,th
        | MTile.TILE_RIGHT -> 240, 80,  tw,th
        | MTile.TILE_BOTTOMRIGHT -> 240, 160,  tw,th
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