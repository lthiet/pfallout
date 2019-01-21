(* Modules *)
(* Utils *)
open Tsdl
open Sdl_tools
(* Assets *)
open GameObject
open Item


(* Constants *)
let screen_width = 1920
let screen_height = 1080

(* Variables *)
(* Events *)
let ev = Some (Sdl.Event.create ())

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

(* Functions *)
(* Initialize a window and a surface *)
let initialization () = 
    (* Initialize SDL *)
    manage_result ( Sdl.init Sdl.Init.everything ) "Error init : %s";

    (* Open a Window *)
    let window = manage_result (Sdl.create_window "TRPG" ~w:screen_width ~h:screen_height Sdl.Window.windowed ) "Error create window : %s" in

    (* Get renderer from Window *)
    let create_renderer_flag = (Sdl.Renderer.(+)) Sdl.Renderer.accelerated Sdl.Renderer.presentvsync in 
    let renderer = manage_result (Sdl.create_renderer ~index:(-1) ~flags:create_renderer_flag window) "Error create renderer : %s" in

    (* Get surface from Window *)
    let screen_surface = manage_result (Sdl.get_window_surface window) "Error create surface from window : %s" in
    window,screen_surface,renderer

(* load an image at specified path*)
let load_surface screen_surface path = 
    let loaded_surface = manage_result (Sdl.load_bmp path) "Error opening bitmap : %s" in
    let surface_format_enum = Sdl.get_surface_format_enum screen_surface in
    let optimized_surface = manage_result (Sdl.convert_surface_format loaded_surface surface_format_enum) "Error convert surface : %s" in
    Sdl.free_surface loaded_surface;
    optimized_surface

(* safely close all the windows and surfaces *)
let close windows surfaces renderers =
    List.iter ( fun x -> Sdl.destroy_window x ) windows;
    List.iter ( fun x -> Sdl.free_surface x ) surfaces;
    List.iter ( fun x -> Sdl.destroy_renderer x ) renderers

(* Load all the images related to the game *)
let load_media screen_surface =
    Hashtbl.add key_press_surfaces KEY_PRESS_SURFACE_DEFAULT (load_surface screen_surface "asset/image/just.bmp");
    Hashtbl.add key_press_surfaces KEY_PRESS_SURFACE_UP (load_surface screen_surface "asset/image/up.bmp");
    Hashtbl.add key_press_surfaces KEY_PRESS_SURFACE_DOWN (load_surface screen_surface "asset/image/down.bmp");
    Hashtbl.add key_press_surfaces KEY_PRESS_SURFACE_RIGHT (load_surface screen_surface "asset/image/right.bmp");
    Hashtbl.add key_press_surfaces KEY_PRESS_SURFACE_LEFT (load_surface screen_surface "asset/image/left.bmp")

let rec game renderer surface over =
    if  over then
        ()
    else
    let new_surface, new_over = 
        (* Get the next event in the queue *)
        if not (Sdl.poll_event ev) then (
            match ev with
            (* If no event, nothing to do *)
            | None ->
                surface,over
            (* Otherwise, check the event *)
            | Some e ->
                (* If the user clicks the red cross button, the game closes *)
                if (Sdl.Event.get e Sdl.Event.typ) = Sdl.Event.quit then
                    surface, true
                (* Else, check if it is a key down *)
                else if Sdl.Event.get e Sdl.Event.typ = Sdl.Event.key_down then (
                    (* Get the key that was pressed *)
                    let pressed_key = Sdl.Event.get e Sdl.Event.keyboard_keycode in 
                    (* Quit in case the player presses escape *)
                    if pressed_key = Sdl.K.escape then
                        surface, true
                    else
                        (* Check which key was pressed and select the image accordingly *)
                        let which_surface = if pressed_key = Sdl.K.up then
                            KEY_PRESS_SURFACE_UP
                        else if pressed_key = Sdl.K.down then
                            KEY_PRESS_SURFACE_DOWN
                        else if pressed_key = Sdl.K.left then
                            KEY_PRESS_SURFACE_LEFT
                        else if pressed_key = Sdl.K.right then
                            KEY_PRESS_SURFACE_RIGHT
                        else
                            KEY_PRESS_SURFACE_DEFAULT
                        in
                        (Hashtbl.find key_press_surfaces which_surface), over
                    ) else ( surface,over)
            ) else (surface,over) in

            (* Get the texture from the surface *)
            let current_texture =
                manage_result (
                    Sdl.create_texture_from_surface renderer new_surface 
                    ) "Error create texture from surface : %s"
                    in

            (* Load the renderer with the texture *)
            manage_result (
                Sdl.render_copy renderer current_texture
                ) "Error render copy : %s";

            (* Update the renderer *)
            Sdl.render_present renderer;

            (* Clear the texture *)
            Sdl.destroy_texture current_texture;

            (* Continue the game *)
            game renderer new_surface new_over

let machin = GameObject.create_game_object 1 2 3
let item_machin = Item.create_item 10 2 3 10 50

(* Main  *)
let () =
    let window,screen_surface,renderer = initialization () in
    load_media screen_surface;
    let current_surface = Hashtbl.find key_press_surfaces KEY_PRESS_SURFACE_DEFAULT in
    game renderer current_surface false;
    close [window] [] [];

    Printf.printf "%d" (Item.get_x item_machin)
