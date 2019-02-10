open Grid
open Action_enum

module MAction = struct
    type t = MAction_enum.t * MGrid.t
end
;;