(* Only concerns animation *)

open Entity
open Grid

module MAnimation = struct
    type frame = int
    type t = {
        to_be_animated : (MEntity.t * int) list;
        over : bool
    }
end
;;