open Utils
open Entity
open Entity_enum
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

  (* Get the healthbar of an entity *)
  let get_healthbar_of_entity entity = 
    let max_hp = MEntity_enum.max_hp_of entity#get_ut in
    let current_hp = entity#get_hp in
    create max_hp current_hp

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
end