(* Modules *)
open Tsdl.Sdl.Init
open Tsdl.Sdl

(* Constants *)
let screen_width = 640
let screen_height = 480

exception WTF

let () =
    (* Initialize SDL *)
    let flag_init = Tsdl.Sdl.Init.everything in
    let result_init = Tsdl.Sdl.init flag_init in 
    match result_init with
    | Error ( `Msg e ) ->  Tsdl.Sdl.log "Init error : %s" e;exit 1
    | Ok () ->
        (* Open a Window *)
        let flag_create_window = Tsdl.Sdl.Window.windowed in
        let result_create_window = Tsdl.Sdl.create_window "main" ~w:screen_width ~h:screen_height flag_create_window in
        match result_create_window with
        | Error ( `Msg e ) ->  Tsdl.Sdl.log "Init error : %s" e;exit 1
        | Ok a ->
            Printf.printf "ok"

