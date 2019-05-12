open Utils
open Sdl_tools
open Hex
open Colors

module MHealthbar : 
sig
  val healthbar_length : int
  type t
  val create : int -> int -> t
  val render : Tsdl.Sdl.renderer -> float -> int -> int -> t -> unit
end