open Tsdl
open Tsdl_ttf

module MFont_pack : 
sig
  val good_times : string
  val open_font : string -> int -> Tsdl_ttf.Ttf.font
  val open_font_with_outline : string -> int -> int -> Tsdl_ttf.Ttf.font * Tsdl_ttf.Ttf.font
end