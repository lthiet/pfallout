open Tsdl
open Texture_wrapper
open Binder
open Tile

(* Constants *)
module GridGraphics = struct
    let init renderer =
        TileGraphics.init renderer

    let render renderer camera = 
        List.iter (fun x ->
            TileGraphics.render renderer x camera
        ) !(TileGraphics.tiles)
end
;;