(* Modules *)
open Tsdl.Sdl

(* Constants *)
let screen_width = 640
let screen_height = 480
let bg_path = "./asset/image/just.bmp"


exception WTF

(* Utils function *)
let manage_result r s =
    match r with
    | Error ( `Msg e ) ->  Tsdl.Sdl.log s e;exit 1
    | Ok a -> a

exception NonePointer

let deref_option p =
    match !p with
    | Some x -> x
    | None -> raise NonePointer

(* Main functions *)
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

let load_bmp string =
    let result = Tsdl.Sdl.load_bmp string in
    manage_result result "Load BMP error : %s"

let blit_surface src rect_src dst rect_dst =
    let result = Tsdl.Sdl.blit_surface src rect_dst dst rect_dst in
    manage_result result "Blit surface error : %s"

let () =
    (* Pointers *)
    let window_p = ref None in
    let surface_p = ref None in
    let bg_p = ref None in

    (* Initialize a window and a surface *)
    let initialization () = 
        (* Initialize SDL *)
        init ();

        (* Open a Window *)
        window_p := Some (create_window "TRPG" screen_width screen_height);

        (* Get surface from Window *)
        let window = deref_option window_p in
        surface_p := Some (get_window_surface window)
    in

    let load_media () = 
        bg_p := Some (load_bmp bg_path)
    in

     (* Safely close the program *)
    let close () =
        Tsdl.Sdl.free_surface (deref_option surface_p);
        surface_p := None;
        Tsdl.Sdl.free_surface (deref_option bg_p);
        bg_p := None;
        Tsdl.Sdl.destroy_window (deref_option window_p);
        window_p := None
    in


    initialization ();

    load_media ();

    blit_surface (deref_option bg_p) None (deref_option surface_p) None;
    update_window_surface (deref_option window_p);

    Tsdl.Sdl.delay (Int32.of_int 2000);

    close ();