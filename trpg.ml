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

let fill_rect surface rect color =
    let result = Tsdl.Sdl.fill_rect surface rect color in
    manage_result result "Fill rect error : %s"

let alloc_format format_enum = 
    let result = Tsdl.Sdl.alloc_format format_enum in
    manage_result result "Allocate format error : %s"

let update_window_surface window =
    let result = Tsdl.Sdl.update_window_surface window in
    manage_result result "Update window surface error : %s"

let () =
    (* Initialize SDL *)
    init ();
    (* Open a Window *)
    let window = create_window "TRPG" screen_width screen_height in

    (* Get surface from Window *)
    let surface = get_window_surface window in

    (* Get RGB for white *)
    let format_enum = Tsdl.Sdl.get_surface_format_enum surface in
    let pixel_format = alloc_format format_enum in
    let color = Tsdl.Sdl.map_rgb pixel_format 255 255 255 in

    (* Fill rect on the window*)
    fill_rect surface None color;

    (* Update the surface *)
    update_window_surface window;

    (* Delay *)
    Tsdl.Sdl.delay (Int32.of_int 2000);
