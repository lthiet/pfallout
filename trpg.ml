(* Modules *)
open Tsdl
open Cell
open Sdl_tools

(* Constants *)
let screen_width = 1920
let screen_height = 1080

(* Variables *)
(* The window *)
let window_p = ref None 
(* The renderer *)
let renderer_p = ref None
(* The surface that is currently being displayed *)
let current_surface_p = ref None 
(* Events *)
let ev = Some (Sdl.Event.create ())

(* Lists *)
(* Windows list *)
let windows_p = [window_p]
(* Surfaces list not associated with window *)
let surfaces_p = [current_surface_p]

(* Types *)
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
let manage_result r s =
    match r with
    | Ok a -> a
    | Error (`Msg e) -> Sdl.log s e;exit 1

exception NonePointer

(* Derefenrece an option variable pointed by p *)
let deref_option p =
    match !p with
    | Some x -> x
    | None -> raise NonePointer

(* Functions *)
(* Initialize a window and a surface *)
let initialization () = 
    (* Initialize SDL *)
    manage_result ( Sdl.init Sdl.Init.everything ) "Error init : %s";

    (* Open a Window *)
    window_p := Some ( manage_result (Sdl.create_window "TRPG" ~w:screen_width ~h:screen_height Sdl.Window.windowed ) "Error create window : %s");

    (* Get renderer from Window *)
    let window = deref_option window_p in
    let create_renderer_flag = (Sdl.Renderer.(+)) Sdl.Renderer.accelerated Sdl.Renderer.presentvsync in 
    renderer_p := Some (manage_result (Sdl.create_renderer ~index:(-1) ~flags:create_renderer_flag window) "Error create renderer : %s");
    ()

(* load an image at specified path*)
let load_surface path = 
    let loaded_surface = manage_result (Sdl.load_bmp path) "Error opening bitmap : %s" in
    let surface_format_enum = Sdl.get_surface_format_enum (deref_option current_surface_p)
    let optimized_surface = Sdl.convert_surface loaded_surface

(* safely close all the windows and surfaces *)
let close () =
    List.iter ( fun x -> Sdl.destroy_window (deref_option x); x:= None) windows_p;
    List.iter ( fun x -> Sdl.free_surface (deref_option x); x:= None) surfaces_p

(* Load all the images related to the game *)
let load_media () =
    Hashtbl.add key_press_surfaces KEY_PRESS_SURFACE_DEFAULT (load_surface "asset/image/just.bmp");
    Hashtbl.add key_press_surfaces KEY_PRESS_SURFACE_UP (load_surface "asset/image/up.bmp");
    Hashtbl.add key_press_surfaces KEY_PRESS_SURFACE_DOWN (load_surface "asset/image/down.bmp");
    Hashtbl.add key_press_surfaces KEY_PRESS_SURFACE_RIGHT (load_surface "asset/image/right.bmp");
    Hashtbl.add key_press_surfaces KEY_PRESS_SURFACE_LEFT (load_surface "asset/image/left.bmp")



(* Main  *)
let () =
    initialization ();
    load_media ();
    current_surface_p := Some (Hashtbl.find key_press_surfaces KEY_PRESS_SURFACE_DEFAULT);

    (* Main function that implements the mechanism of the game *)
    let rec game b =
        (* Bool pointer that tells whether or not the game is over *)
        let over = ref false in
        if not b then (

            (* Get the next event in the queue *)
            if not (Sdl.poll_event ev) then (
                match ev with

                (* If no event, nothing to do *)
                | None ->
                    ()
                (* Otherwise, check the event *)
                | Some e ->

                    (* If the user clicks the red cross button, the game closes *)
                    if (Sdl.Event.get e Sdl.Event.typ) = Sdl.Event.quit then
                        over := true

                    (* Else, check if it is a key down *)
                    else if Sdl.Event.get e Sdl.Event.typ = Sdl.Event.key_down then (
                        (* Get the key that was pressed *)
                        let pressed_key = Sdl.Event.get e Sdl.Event.keyboard_keycode in

                        (* Check which key was pressed and select the image accordingly *)
                        if pressed_key = Sdl.K.up then
                            current_surface_p := Some (Hashtbl.find key_press_surfaces KEY_PRESS_SURFACE_UP)
                        else if pressed_key = Sdl.K.down then
                            current_surface_p := Some (Hashtbl.find key_press_surfaces KEY_PRESS_SURFACE_DOWN)
                        else if pressed_key = Sdl.K.left then
                            current_surface_p := Some (Hashtbl.find key_press_surfaces KEY_PRESS_SURFACE_LEFT)
                        else if pressed_key = Sdl.K.right then
                            current_surface_p := Some (Hashtbl.find key_press_surfaces KEY_PRESS_SURFACE_RIGHT)

                        (* Quit in case the player presses escape *)
                        else if pressed_key = Sdl.K.escape then
                            over := true
                        else
                            current_surface_p := Some (Hashtbl.find key_press_surfaces KEY_PRESS_SURFACE_DEFAULT)
                    );
            );

            (* Get the texture from the surface *)
            let current_texture =
                manage_result (
                    Sdl.create_texture_from_surface (deref_option renderer_p) (deref_option current_surface_p) 
                    ) "Error create texture from surface : %s"
                    in

            (* Load the renderer with the texture *)
            manage_result (
                Sdl.render_copy (deref_option renderer_p) current_texture
                ) "Error render copy : %s";

            (* Update the renderer *)
            Sdl.render_present (deref_option renderer_p);

            (* Continue the game *)
            game !over
        )
        else ()
    in
    game false;
    close ();