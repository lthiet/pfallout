open Action_enum
open Entity
open Texture_wrapper
open Utils
open Hex
open Tsdl

module MMilitary = struct

  let create_soldier r q f = 
    MEntity.create r q 30 20 3 3 40 25 1 [MAction_enum.PASS;MAction_enum.MOVE;MAction_enum.ATTACK] [MAction_enum.REFILL_MP] f SOLDIER MELEE GROUND 3

  let clip = Sdl.Rect.create 0 0 MHex.width MHex.height
end
;;