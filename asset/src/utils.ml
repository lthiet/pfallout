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

let check_collision a b =
  let la = Sdl.Rect.x a in
  let lb = Sdl.Rect.x b in
  let ra = la + Sdl.Rect.w a in
  let rb = lb + Sdl.Rect.w b in
  let ta = Sdl.Rect.y a in
  let tb = Sdl.Rect.y b in
  let ba = ta + Sdl.Rect.h a in
  let bb = tb + Sdl.Rect.h b in
  if ba <= tb || ta >= bb || ra <= lb || la >= rb then
    false
  else
    true

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

(* Increment a refernce and returns the new value *)
let incr pt =
  begin
    pt := !pt + 1;
    !pt
  end

let debug str =
  print_newline ();
  print_string str;

