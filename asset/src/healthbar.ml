open Utils
open Sdl_tools
open Hex
open Colors
(* This module implements the healthbar on the units *)

module MHealthbar = struct
  let healthbar_length = 200 

  type t = {
    max_hp : int;
    current_hp : int;
  }

  let create max_hp current_hp =
    {
      max_hp = max_hp;
      current_hp = current_hp
    }


  let to_couple t = (t.max_hp,t.current_hp)

  (* Returns the percentage of current health *)
  let hp_ratio t =
    let mh,ch = to_couple t in
    (* Health value converted to float *)
    let mh_f,ch_f = (float_of_int mh),(float_of_int ch) in

    (* Compute the percentage *)
    ch_f/.mh_f


  let hp_ratio_int t =
    hp_ratio t *. (float_of_int healthbar_length) |> round

  (* Get the green bar length of the healthbar, green bar represents the current hp *)
  let get_green_bar_length t =
    min healthbar_length (hp_ratio_int t)

  (* Get the red bar length of the healthbar, red bar represents the missing hp *)
  let get_red_bar_length t = 
    let ratio = hp_ratio_int t in
    max (healthbar_length-ratio) (0)

  (* Get the blue bar length of the healthbar, red bar represents the missing hp *)
  let get_blue_bar_length t =
    let ratio = hp_ratio_int t in
    min healthbar_length (max (ratio-healthbar_length) (0))

  let render renderer scale x y healthbar = 
    let rbl = get_red_bar_length healthbar in
    let gbl = get_green_bar_length healthbar in
    let bbl = get_blue_bar_length healthbar in

    let y = (y + 40) in
    let x = (x + (MHex.width/2) - (healthbar_length / 2)) in

    let bottom = scale_to (y+10) scale in
    let top = scale_to (y) scale in
    let left = scale_to x scale in
    let left_red = scale_to (x+healthbar_length) scale in
    let right_green = scale_to (x+gbl) scale in
    let right_red = scale_to (x+healthbar_length-rbl) scale in
    let right_blue = scale_to (x+bbl) scale in

    (* First render the green bar *)
    manage_result (draw_filled_rectangle renderer (MColor.to_quadruple MColor.green) (bottom, top, left, right_green)) "Error : %s";

    (* Then render the red bar *)
    manage_result (draw_filled_rectangle renderer (MColor.to_quadruple MColor.red) (bottom, top, left_red, right_red)) "Error : %s";

    (* Finally render the blue bar *)
    manage_result (draw_filled_rectangle renderer (MColor.to_quadruple MColor.blue) (bottom, top, left, right_blue)) "Error : %s";
end