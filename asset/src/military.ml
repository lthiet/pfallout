open Action_enum
open Entity
open Utils
open Hex
open Tsdl
open Behaviour_enum
open Entity_enum

(* Create entities that are militaries *)
module MMilitary = struct

  let create_soldier r q f = 
    MEntity.create r q MEntity_enum.max_hp_of_soldier 20 MEntity_enum.max_mp_of_soldier 25 10 1 [MAction_enum.PASS_E;MAction_enum.MOVE_E;MAction_enum.ATTACK_E] [MAction_enum.REFILL_MP_E] f SOLDIER MILITARY MELEE GROUND 3 MBehaviour_enum.WANDERING

  let clip = Sdl.Rect.create 0 0 MHex.width MHex.height
end
;;