open Utils
open Sdl_tools
open Hex
(* This module implements the healthbar on the units *)

module MHealthbar = struct
  let healthbar_length = 100 

  type t = {
    max_hp : int;
    current_hp : int;
  }

  let create max_hp current_hp =
    {
      max_hp = max_hp;
      current_hp = current_hp
    }

  let red = 255,0,0,255
  let green = 0,255,0,255
  let blue = 21, 130, 181,255

  let to_couple t = (t.max_hp,t.current_hp)

  (* Returns the percentage of current health *)
  let hp_ratio t =
    let mh,ch = to_couple t in
    (* Health value converted to float *)
    let mh_f,ch_f = (float_of_int mh),(float_of_int ch) in

    (* Compute the percentage *)
    ch_f/.mh_f


  let hp_ratio_int t =
    hp_ratio t *. 100. |> round

  (* Get the green bar length of the healthbar, green bar represents the current hp *)
  let get_green_bar_length t =
    min healthbar_length (hp_ratio_int t)

  (* Get the red bar length of the healthbar, red bar represents the missing hp *)
  let get_red_bar_length t = 
    let ratio = hp_ratio_int t in
    max (100-ratio) (0)

  (* Get the blue bar length of the healthbar, red bar represents the missing hp *)
  let get_blue_bar_length t =
    let ratio = hp_ratio_int t in
    min healthbar_length (max (ratio-100) (0))

  let render renderer x y healthbar = 
    let rbl = get_red_bar_length healthbar in
    let gbl = get_green_bar_length healthbar in
    let bbl = get_blue_bar_length healthbar in

    let y = y + 40 in
    let x = x + (MHex.width/2) - (healthbar_length / 2) in

    (* First render the green bar *)
    manage_result (draw_filled_rectangle renderer green (y+10, y, x, x+gbl)) "Error : %s";

    (* Then render the red bar *)
    manage_result (draw_filled_rectangle renderer red (y+10, y, x+healthbar_length, x+healthbar_length-rbl)) "Error : %s";

    (* Finally render the blue bar *)
    manage_result (draw_filled_rectangle renderer blue (y+10, y, x, x+bbl)) "Error : %s";
end