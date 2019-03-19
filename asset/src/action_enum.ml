open Tsdl
open Hex
open Layer_enum
open Entity_enum
open Item

module MAction_enum = struct

  (* This enum is used for knowing which actions are on done on start or
     which action are allowed for an entity *)
  type enum = ATTACK_E
            | MOVE_E
            | REFILL_MP_E
            | PASS_E
            | CHANGE_BEHAVIOUR_E
            | SPAWN_ENTITY_E
            | USE_ITEM_E

  type t = ATTACK of MHex.axial_coord * MHex.axial_coord * MLayer_enum.t * MLayer_enum.t
         | MOVE of MHex.axial_coord * MHex.axial_coord * MLayer_enum.t 
         | REFILL_MP of MHex.axial_coord * MLayer_enum.t
         | PASS of MHex.axial_coord * MLayer_enum.t
         | CHANGE_BEHAVIOUR of MHex.axial_coord * MLayer_enum.t
         | SPAWN_ENTITY of MHex.axial_coord * MHex.axial_coord * MEntity_enum.t
         | USE_ITEM of MItem.code * MItem.param

  exception Invalid_on_start_action

  (* Create an action t if it was an action on start *)
  let action_on_start t entity =
    match t with
    | REFILL_MP_E -> REFILL_MP (entity#get_axial,entity#get_lt)
    |  _ -> raise Invalid_on_start_action

  let create_move src dst layer =
    MOVE (src,dst,layer)

  let create_attack src dst src_layer dst_layer =
    ATTACK (src,dst,src_layer,dst_layer)

  let create_pass src layer =
    PASS (src,layer)

  let create_spawn_entity src dst entity =
    SPAWN_ENTITY (src,dst,entity)

  exception Unknown_Action

  let to_str t =
    match t with
    | ATTACK _-> "ATTACK"
    | MOVE _ -> "MOVE"
    | REFILL_MP _-> "REFILL_MP"
    | PASS _ -> "PASS"
    | CHANGE_BEHAVIOUR _ -> "CHANGE_BEHAVIOUR"
    | SPAWN_ENTITY _ -> "SPAWN_SOLDIER"
    | USE_ITEM _ -> "USE_ITEM"

  let print t =
    match t with
    | None ->
      Printf.printf "%s\n" "None";
    | Some e ->
      Printf.printf "%s\n" (to_str e)

  exception No_key_assigned

  let action_to_key = function
    | ATTACK _ -> Sdl.Scancode.o
    | MOVE _ -> Sdl.Scancode.p
    | _ -> raise No_key_assigned
end
;;