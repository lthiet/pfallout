open Tsdl

let () = Random.self_init ()

(* Utils function *)
(* Manage Result *)
let manage_result r s =
  match r with
  | Ok a -> a
  | Error (`Msg e) -> Sdl.log s e;exit 1

(* Manage Option *)
let manage_option r s =
  match r with
  | Some x -> x
  | None -> Sdl.log s;exit 1

(* Get format field from a surface *)
let get_format_from_surface surface =
  let enum = Sdl.get_surface_format_enum surface in
  manage_result (
    Sdl.alloc_format enum
  ) "Error alloc format %s"

(* Make a rectangle *)
let make_rect x y w h =
  Sdl.Rect.create x y w h 

(* Check if key 'i' has been pressed *)
let check_key_scan ks i =
  (ks.{i} = 1)

(* Check event type *)
let check_ev_type e t =
  Sdl.Event.get e Sdl.Event.typ = t

let char_is_number c =
  match c with
  | '0'
  | '1'
  | '2'
  | '3'
  | '4'
  | '5'
  | '6'
  | '7'
  | '8'
  | '9' -> true
  | _ -> false

let round f = 
  truncate (floor (f +. 0.5))

(* Compute a new coordinates,
    pk : pressed_key
    ekp : expected key positive
    ekn : expected key negative
*)
let new_int pk ekp ekn offset old =
  if pk = ekp then
    old + offset
  else if pk = ekn then
    old - offset
  else
    old

(* Put the first at the last position of a list, and the second at the first and so on *)
let cycle l =
  match l with
  | [] -> []
  | x :: s -> s @ [x]

exception Empty_list

(* NB: this is unefficient, consider deprecating this in the future *)
let random_elem_list l =
  match l with
  | [] -> raise Empty_list
  | _ -> List.nth l (Random.int (List.length l))

(* Exception whenever a function has not been implement yet
   but requires to be called nonetheless *)
exception Not_yet_implemented 

(* Exception for debugging *)
exception Debugging 

(* Increment a refernce and returns the new value *)
let incr pt =
  begin
    pt := !pt + 1;
    !pt
  end

let rec remove_f l acc f =
  match l with
  | [] -> acc
  | x :: s -> 
    if f x then
      remove_f s acc f
    else
      remove_f s (x :: acc) f

let scale_to x r =
  round ((float_of_int x) *. r)

let inverse r = 1. /. r

let scale_rect rect r =
  let x,y,w,h =
    Sdl.Rect.x rect,
    Sdl.Rect.y rect,
    Sdl.Rect.w rect,
    Sdl.Rect.h rect
  in
  let x',y',w',h' =
    scale_to x r,
    scale_to y r,
    scale_to w r,
    scale_to h r
  in
  Sdl.Rect.create x' y' w' h'



let check_collision a b =
  Sdl.has_intersection a b

let rect_to_string rect =
  let x,y,w,h =
    Sdl.Rect.x rect,
    Sdl.Rect.y rect,
    Sdl.Rect.w rect,
    Sdl.Rect.h rect
  in
  "x :" ^ (string_of_int x) ^ " " ^
  "y :" ^ (string_of_int y) ^ " " ^
  "w :" ^ (string_of_int w) ^ " " ^
  "h :" ^ (string_of_int h) ^ " "

let division_eclid r q =
  r/q,r mod q



let debug str =
  print_newline ();
  print_string str;

