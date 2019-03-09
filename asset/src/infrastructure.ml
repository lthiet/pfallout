open Action_enum
open Behaviour_enum
open Entity
open Utils
open Hex
open Tsdl

module MInfrastructure = struct

  let create_city r q f = 
    MEntity.create r q 100 100 1 1 50 50 2 [MAction_enum.PASS;MAction_enum.ATTACK] [MAction_enum.REFILL_MP] f CITY INFRASTRUCTURE RANGED GROUND (-1) MBehaviour_enum.SPAWNING

  let clip = Sdl.Rect.create 0 0 MHex.width MHex.height
end
;;