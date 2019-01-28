open Tsdl
open Binder

class tile id x y z =
object
    val id : int = id
    val x : int = x
    val y : int = y 
    val z : int = z
    method get_id = id
    method get_x = x
    method get_y = y
    method get_z = z


end
;;

module ELT = struct
    type t = tile
end
;;

module TileRender = MTextureBind(ELT)

let truc = new tile 0 0 0 0