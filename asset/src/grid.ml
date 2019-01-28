open Tsdl
open Texture_wrapper
open Binder
open Tile

(* Constants *)
module Mgrid = struct
    type grid = (tile option) array array array

    let width = 1280
    let height = 960

    let tile_width = 80
    let tile_height = 80

    let total_tiles = 192
    let total_tiles_sprites = 12

    type tile_sprite_enum =
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

end
;;