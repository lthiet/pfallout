open Action_enum
open Entity
open Texture_wrapper
open Utils
open Hex
open Tsdl

module MMilitary = struct
 
  let create_soldier r q f = type military_type = SOLDIER | SNIPER
  type attack_type = MELEE | RANGED
  
    new military r q 30 20 4 4 40 25 1 [MAction_enum.MOVE;MAction_enum.ATTACK] [MAction_enum.REFILL_MP] f SOLDIER MELEE GROUND 3

  let clip = Sdl.Rect.create 0 0 MHex.width MHex.height
end
;;