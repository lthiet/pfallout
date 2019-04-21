open Entity
open Grid
open Hex
open Fx
open Utils

module MAnimation = struct
  type frame = int

  type animation_unit = {
    entity : MEntity.t;
    fx : MFx.t option;
    initial_time : int;
    current_time : int
  }

  let create_animation_unit entity fx time = {
    entity = entity;
    fx = fx;
    initial_time = time;
    current_time = time;
  }

  let get_entity au = au.entity
  let get_fx au = au.fx
  let get_initial_time au = au.initial_time
  let get_current_time au = au.current_time

  type t = {
    to_be_animated : animation_unit list list;
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
    current_fx : MFx.t option;
    next_animated : MEntity.t option;
    time_left : int;
    initial_time_left : int;
  }

  let get_currently_animated t = t.currently_animated
  let get_current_fx t = t.current_fx
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
        | au :: [] -> {
            currently_animated = au.entity;
            next_animated = None;
            current_fx = au.fx;
            time_left = au.current_time;
            initial_time_left = au.initial_time;
          } :: acc
        | au1 :: au2 :: s  -> {
            currently_animated = au1.entity;
            next_animated = Some (au2.entity);
            current_fx = au1.fx;
            time_left = au1.current_time;
            initial_time_left = au1.initial_time;
          } :: acc
    ) [] t.to_be_animated

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
      List.fold_right (
        fun x acc1 -> 
          let tmp = match x with
            | [] -> []
            | au :: s->  
              let new_frame = au.current_time - 1 in
              if new_frame > 0 then
                {
                  au with current_time = new_frame
                }
                :: s
              else
                s
          in
          match tmp with
          | [] -> acc1
          | _ -> tmp :: acc1
      ) l []
    in

    {
      to_be_animated = new_tba
    }

  (* Some functions to create specific animations *)
  let create_nuke_drop src_ent dst victims height_drop duration_drop duration_damage =

    let length_drop = height_drop / duration_drop in
    let drop_animation = 
      (* The list of anims for the victims *)
      let l1,src_in_victim = List.fold_left ( 
          fun (acc,src_in_victim) x -> 
            let au_drop = create_animation_unit x None length_drop in
            let au_attacked = create_animation_unit x (Some(MFx.create_from_entity x MFx.ATTACKED)) duration_damage in
            (
              ([au_drop;au_attacked]):: acc),(src_in_victim || x = src_ent)
        ) ([],false) victims
      in

      (* The list of the src if not in the victims *)
      let l2 = if not src_in_victim then
          [create_animation_unit src_ent None (length_drop + duration_damage)]
        else
          []
      in

      (* The bomb dropping *)
      let l3 = 
        let screen_x,screen_y = MHex.axial_to_screen_coord dst in
        List.init length_drop ( fun i -> create_animation_unit (MEntity.create_fx_binder ()) (Some(MFx.create MFx.NUKE_DROP screen_x (screen_y - height_drop +(duration_drop*i)))) 1) 
      in
      l3 :: l2 :: l1
    in
    create drop_animation



end
;;