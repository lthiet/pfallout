open Utils
open Sdl_tools
open Hex
open Colors
(* This module implements the healthbar on the units *)

module MHealthbar : sig
  val healthbar_length : int
  type t
  val create : int -> int -> t
  val render : Tsdl.Sdl.renderer -> float -> int -> int -> t -> unit
end