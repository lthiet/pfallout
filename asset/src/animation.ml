(* Only concerns animation *)

open Entity
open Grid

module MAnimation = struct
  type frame = int
  type t = {
    to_be_animated : (MEntity.t * int) list list;
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


  let print t =
    List.iter (
      fun x -> 
        List.iter ( fun (y,z) -> 
            Printf.printf "%d %d - %d /" y#get_r y#get_q z
          ) x;
        print_newline ();
    ) t.to_be_animated

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

  let get_current_animated t = 
    List.fold_left (
      fun acc x ->
        match x with
        | [] -> acc
        | (entity,_) :: s -> entity :: acc
    )
      [] t.to_be_animated

  let compute_next t =
    let l = t.to_be_animated in

    let new_tba =
      List.fold_left (
        fun acc1 x -> 
          let tmp = match x with
            | [] -> []
            | (x,y) :: s->  
              let new_frame = y - 1 in
              if new_frame > 0 then
                (x,new_frame) :: s
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