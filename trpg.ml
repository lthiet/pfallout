(* Modules *)
open Tsdl

(* Constants *)
let screen_width = 640
let screen_height = 480
let bg_path = "./asset/image/just.bmp"

(* Variables *)
(* The window *)
let window_p = ref None 
(* The surface contained by the window *)
let screen_surface_p = ref None 
(* The surface that is currently being displayed *)
let current_surface_p = ref None 
(* Events *)
let ev = Some (Sdl.Event.create ())

(* Lists *)
(* Windows list *)
let windows_p = [window_p]
(* Surfaces list not associated with window *)
let surfaces_p = [current_surface_p]

(* Type *)
type key_pressed_surfaces_enum =
    | KEY_PRESS_SURFACE_DEFAULT
    | KEY_PRESS_SURFACE_UP
    | KEY_PRESS_SURFACE_DOWN
    | KEY_PRESS_SURFACE_LEFT
    | KEY_PRESS_SURFACE_RIGHT
    | KEY_PRESS_SURFACE_TOTAL

(* Hashtbl *)
let key_press_surfaces = Hashtbl.create 7

(* Utils function *)
(* Manage Result *)
let manage_result r =
    match r with
    | Ok a -> a
    | Error (`Msg e) -> Sdl.log "Error : %s" e;exit 1

exception NonePointer

let deref_option p =
    match !p with
    | Some x -> x
    | None -> raise NonePointer

(* Functions *)
(* Initialize a window and a surface *)
let initialization window_p surface_p = 
    (* Initialize SDL *)
    manage_result ( Sdl.init Sdl.Init.everything );

    (* Open a Window *)
    window_p := Some ( manage_result (Sdl.create_window "TRPG" ~w:screen_width ~h:screen_height Sdl.Window.windowed ));

    (* Get surface from Window *)
    let window = deref_option window_p in
    surface_p := Some (manage_result (Sdl.get_window_surface window));
    ()

(* load an image at specified path*)
let load_surface path = 
    manage_result (Sdl.load_bmp path)

(* safely close all the windows and surfaces *)
let close () =
    List.iter ( fun x -> Sdl.destroy_window (deref_option x); x:= None) windows_p;
    List.iter ( fun y -> Sdl.free_surface (deref_option y ); y:= None) surfaces_p

let load_media () =
    Hashtbl.add key_press_surfaces KEY_PRESS_SURFACE_DEFAULT (load_surface "asset/image/just.bmp");
    Hashtbl.add key_press_surfaces KEY_PRESS_SURFACE_UP (load_surface "asset/image/up.bmp");
    Hashtbl.add key_press_surfaces KEY_PRESS_SURFACE_DOWN (load_surface "asset/image/down.bmp");
    Hashtbl.add key_press_surfaces KEY_PRESS_SURFACE_RIGHT (load_surface "asset/image/right.bmp");
    Hashtbl.add key_press_surfaces KEY_PRESS_SURFACE_LEFT (load_surface "asset/image/left.bmp")



(* Main  *)
let () =
   
    initialization window_p screen_surface_p;

    load_media ();

    current_surface_p := Some (Hashtbl.find key_press_surfaces KEY_PRESS_SURFACE_DEFAULT);

    let rec game b =
        let over = ref false in
        if not b then (
            if not (Sdl.poll_event ev) then (
                match ev with
                | Some e ->
                    if (Sdl.Event.get e Sdl.Event.typ) = Sdl.Event.quit then
                        over := true
                    else if Sdl.Event.get e Sdl.Event.typ = Sdl.Event.key_down then (
                        let pressed_key = Sdl.Event.get e Sdl.Event.keyboard_keycode in
                        if pressed_key = Sdl.K.up then
                            current_surface_p := Some (Hashtbl.find key_press_surfaces KEY_PRESS_SURFACE_UP)
                        else if pressed_key = Sdl.K.down then
                            current_surface_p := Some (Hashtbl.find key_press_surfaces KEY_PRESS_SURFACE_DOWN)
                        else if pressed_key = Sdl.K.left then
                            current_surface_p := Some (Hashtbl.find key_press_surfaces KEY_PRESS_SURFACE_LEFT)
                        else if pressed_key = Sdl.K.right then
                            current_surface_p := Some (Hashtbl.find key_press_surfaces KEY_PRESS_SURFACE_RIGHT)
                        else if pressed_key = Sdl.K.escape then
                            over := true
                        else    
                            current_surface_p := Some (Hashtbl.find key_press_surfaces KEY_PRESS_SURFACE_DEFAULT)
                    );
                | None ->
                    ()
            );
            manage_result (Sdl.blit_surface (deref_option current_surface_p) None (deref_option screen_surface_p) None);
            manage_result (Sdl.update_window_surface (deref_option window_p));
            game !over
        )
        else ()
    in

    game false;
    close ()