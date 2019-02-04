open Tsdl
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

    class tile id r q tile_type =
    object(self)
        inherit game_object r q as super
        val tile_type : tile_type = tile_type
        method get_tile_type = tile_type
    end

    (* Functions *)
    let get_screen_x t = let x,_ = MHex.axial_to_screen_coord t#get_axial in x 
    let get_screen_y t = let _,y = MHex.axial_to_screen_coord t#get_axial in y
    let get_box t = Sdl.Rect.create (get_screen_x t) (get_screen_y t) MHex.width MHex.height

    (* Match a tile type to a clip to get the texture from *)
    let match_tile_type_to_clip t =
        let tw,th = MHex.width,MHex.height in
        let x,y,w,h = match t with
        | TILE_GRASSLAND -> 0, 0, tw,th 
        | TILE_DESERT -> 138, 0,  tw,th
        | TILE_SNOW -> 276, 0,  tw,th
        in
        Sdl.Rect.create x y w h

    (* Render a tile *)
    let render renderer tile tile_texture camera =
        if check_collision (get_box tile) camera then
            MTexture.render renderer
            ~clip:( Some (match_tile_type_to_clip tile#get_tile_type))
            ~x:((get_screen_x tile)- Sdl.Rect.x camera)
            ~y:((get_screen_y tile)- Sdl.Rect.y camera)
            tile_texture
        else
            ()
end
;;