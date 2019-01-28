open Tsdl
open Binder
open Game_object

class tile id x y z =
object (self)
    inherit game_object id x y z
end;;

module ELT = struct
    type t = tile
end
;;

module TileRender = MTextureBind(ELT)

let truc = new tile 1 2 3 4

let () =
    Printf.printf "%d" (truc#get_x + truc#get_y + truc#get_z)
