open Entity
open Grid
open Hex
open Fx

module MAnimation = struct
  type frame = int

  type to_animated = ENTITY of MEntity.t | FX of MFx.t
  type t = {
    to_be_animated : (MEntity.t * int * int) list list;
  }

  let add t1 t2 =
    let rec aux l1 l2 acc =
      match l1,l2 with
      | [],[] -> acc
      | x :: s,[] -> acc @ l1
      | [], x :: s -> acc @ l2
      | x :: s,y :: r -> aux s r  ((x @ y) :: acc)
    in
    let tmp = aux t1.to_be_animated t2.to_be_animated [] in
    {
      to_be_animated = tmp
    }


  let create l = {
    to_be_animated = l
  }

  let is_over t =
    match t.to_be_animated with
    | [] -> true
    | _ -> false

  let is_not_over t =
    not (is_over t)

  let get_to_be_animated t = t.to_be_animated

  (* The result that will contains the next elements to be displayed *)
  type res = {
    currently_animated : MEntity.t;
    next_animated : MEntity.t option;
    time_left : int;
    initial_time_left : int;
  }

  let get_currently_animated t = t.currently_animated
  let get_next_animated t = t.next_animated
  let get_time_left t = t.time_left
  let get_initial_time_left t = t.time_left

  (* Returns the next entity to be animated, and also the next entity
     after that one for grid independant rendering *)
  let get_current_animated_and_next t = 
    List.fold_left (
      fun acc x ->
        match x with
        | [] -> acc
        | (entity,n,init_n) :: [] -> {
            currently_animated = entity;
            next_animated = None;
            time_left = n;
            initial_time_left = init_n;
          } :: acc
        | (first_entity,n,init_n) :: (second_entity,_,_) :: s  -> {
            currently_animated = first_entity;
            next_animated = Some second_entity;
            time_left = n;
            initial_time_left = init_n;
          } :: acc
    ) [] t.to_be_animated

  exception Second_is_none

  (* The function returns where the first entity has to be
     in terms of screen coordinates *)
  let next_coord_currently_animated t =
    match t.next_animated with
    | None -> MHex.axial_to_screen_coord t.currently_animated#get_axial
    | Some second -> 
      let fst_x,fst_y = MHex.axial_to_screen_coord t.currently_animated#get_axial in
      let snd_x,snd_y = MHex.axial_to_screen_coord second#get_axial in
      let x = snd_x + ((fst_x - snd_x) / (1 + t.initial_time_left - t.time_left)) in
      let y = snd_y + ((fst_y - snd_y) / (1 + t.initial_time_left - t.time_left)) in
      x,y

  let compute_next t =
    let l = t.to_be_animated in

    let new_tba =
      List.fold_left (
        fun acc1 x -> 
          let tmp = match x with
            | [] -> []
            | (x,y,z) :: s->  
              let new_frame = y - 1 in
              if new_frame > 0 then
                (x,new_frame,z) :: s
              else
                s
          in
          match tmp with
          | [] -> acc1
          | _ -> tmp :: acc1
      ) [] l
    in

    {
      to_be_animated = new_tba
    }
end
;;