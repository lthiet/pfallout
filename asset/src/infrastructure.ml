open Action_enum
open Behaviour_enum
open Entity
open Utils
open Hex
open Tsdl
open Entity_enum

module MInfrastructure = struct

  let create_city r q f with_player = 
    if with_player then
      MEntity.create r q MEntity_enum.max_hp_of_city 100 1 50 50 2 [MAction_enum.PASS_E;MAction_enum.ATTACK_E] [MAction_enum.SPAWN_ENTITY_E;MAction_enum.REFILL_MP_E] f CITY INFRASTRUCTURE RANGED GROUND (-1) MBehaviour_enum.SPAWNING
    else
      MEntity.create r q MEntity_enum.max_hp_of_city 100 1 50 50 2 [MAction_enum.PASS_E;MAction_enum.ATTACK_E] [MAction_enum.REFILL_MP_E] f CITY INFRASTRUCTURE RANGED GROUND (-1) MBehaviour_enum.SPAWNING

  let clip = Sdl.Rect.create 0 0 MHex.width MHex.height
end
;;