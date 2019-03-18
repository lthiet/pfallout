open Tsdl
open Hex

module MAction_enum = struct
  type t = ATTACK of MHex.axial_coord * MHex.axial_coord
         | MOVE
         | PRODUCE
         | REFILL_MP
         | PASS
         | CHANGE_BEHAVIOUR
         | SPAWN_SOLDIER

  exception Unknown_Action

  let to_str t =
    match t with
    | ATTACK -> "ATTACK"
    | MOVE -> "MOVE"
    | PRODUCE -> "PRODUCE"
    | REFILL_MP -> "REFILL_MP"
    | PASS -> "PASS"
    | CHANGE_BEHAVIOUR -> "CHANGE_BEHAVIOUR"
    | SPAWN_SOLDIER -> "SPAWN_SOLDIER"

  let print t =
    match t with
    | None ->
      Printf.printf "%s\n" "None";
    | Some e ->
      Printf.printf "%s\n" (to_str e)

  exception No_key_assigned

  let action_to_key = function
    | ATTACK -> Sdl.Scancode.o
    | MOVE -> Sdl.Scancode.p
    | _ -> raise No_key_assigned
end
;;