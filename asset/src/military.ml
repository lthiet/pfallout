open Action_enum
open Entity
open Utils
open Hex
open Tsdl
open Behaviour_enum

module MMilitary = struct

  let create_soldier r q f = 
    MEntity.create r q 30 20 2 2 40 25 1 [MAction_enum.PASS;MAction_enum.MOVE;MAction_enum.ATTACK] [MAction_enum.REFILL_MP] f SOLDIER MILITARY MELEE GROUND 3 MBehaviour_enum.WANDERING

  let clip = Sdl.Rect.create 0 0 MHex.width MHex.height
end
;;