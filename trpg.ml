(* Modules *)
open Tsdl.Sdl

(* Constants *)
let screen_width = 640
let screen_height = 480

exception WTF

let manage_result r s =
    match r with
    | Error ( `Msg e ) ->  Tsdl.Sdl.log s e;exit 1
    | Ok a -> a


let init () =
    let flag = Tsdl.Sdl.Init.everything in
    let result = Tsdl.Sdl.init flag in 
    manage_result result "Init error : %s"

let create_window title w h =
    let flag = Tsdl.Sdl.Window.windowed in
    let result = Tsdl.Sdl.create_window title ~w:w ~h:h flag in
    manage_result result "Create window error : %s"

let get_window_surface window = 
    let result = Tsdl.Sdl.get_window_surface window in
    manage_result result "Get window surface error : %s"


let () =
    (* Initialize SDL *)
    init ();
    (* Open a Window *)
    let window = create_window "TRPG" screen_width screen_height in
    (* Get surface from Window *)
    let surface = get_window_surface window in
    Printf.printf "ok";

