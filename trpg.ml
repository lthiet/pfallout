(* Modules *)
open Tsdl.Sdl.Init
open Tsdl.Sdl

(* Constants *)
let screen_width = 640
let screen_height = 480

let () =
    let flag = Tsdl.Sdl.Init.everything in
    let result = Tsdl.Sdl.init flag in 
    match result with
    | Ok a -> Printf.printf "ok";
    | Error b -> Printf.printf "pas ok";
