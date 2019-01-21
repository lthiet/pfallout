(* Modules *)
(* Utils *)
open Tsdl
open Tsdl_image
open Sdl_tools
(* Assets *)
open GameObject
open Item


(* Constants *)
let screen_width = 1920
let screen_height = 1920

(* Variables *)
(* Events *)
let ev = Some (Sdl.Event.create ())

(* Types *)

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

(* Functions *)
(* Initialize a window and a renderer *)
let initialization () = 
    (* Initialize SDL *)
    manage_result ( Sdl.init Sdl.Init.video ) "Error init : %s";

    (* Open a Window *)
    let window = manage_result (Sdl.create_window "TRPG" ~w:screen_width ~h:screen_height Sdl.Window.windowed ) "Error create window : %s" in

    (* Get renderer from Window *)
    let create_renderer_flag = (Sdl.Renderer.(+)) Sdl.Renderer.accelerated Sdl.Renderer.presentvsync in 
    let renderer = manage_result (Sdl.create_renderer ~index:(-1) ~flags:create_renderer_flag window) "Error create renderer : %s" in

    (* Set the color of the renderer *)
    manage_result (Sdl.set_render_draw_color renderer 255 255 255 255) "Error set color renderer %s";

    (* Load PNG Loading *)
    let png_load_flags = Image.Init.png in
    let png_init = Image.init png_load_flags in
    if not (Image.Init.eq png_load_flags png_init) then (
        Sdl.log "Error loader png"; exit 1
    )
    else
        window,renderer

(* Load a texture from a path *)
let load_texture renderer path =
    let loaded_surface = manage_result (Image.load path) "Error opening image : %s" in
    let new_texture = Sdl.create_texture_from_surface renderer loaded_surface in
    Sdl.free_surface loaded_surface;
    manage_result new_texture "Error loading texture : %s"

(* safely close all the windows and renders *)
let close windows surfaces renderers textures =
    List.iter ( fun x -> Sdl.destroy_window x ) windows;
    List.iter ( fun x -> Sdl.free_surface x ) surfaces;
    List.iter ( fun x -> Sdl.destroy_renderer x ) renderers;
    List.iter ( fun x -> Sdl.destroy_texture x ) textures;
    Image.quit ();
    Sdl.quit ()

(* Load all the images related to the game *)
let load_media renderer =
    load_texture renderer "asset/image/texture.png"

let make_rect x y w h =
    Sdl.enclose_points [
        Sdl.Point.create x y;
        Sdl.Point.create w y;
        Sdl.Point.create w h;
        Sdl.Point.create x h
      ]


let rec game renderer texture over =
    if  over then
        ()
    else
        (* Get the next event in the queue *)
        let new_texture,new_over =
            if not (Sdl.poll_event ev) then (
                match ev with
                (* If no event, nothing to do *)
                | None ->
                    texture,over
                (* Otherwise, check the event *)
                | Some e -> (
                    (* If the user clicks the red cross button, the game closes *)
                    let over_from_input = if (Sdl.Event.get e Sdl.Event.typ) = Sdl.Event.quit then
                        true
                    else if
                        Sdl.Event.get e Sdl.Event.typ = Sdl.Event.key_down 
                        && Sdl.Event.get e Sdl.Event.keyboard_keycode = Sdl.K.escape then
                        true
                    else
                        false
                    in
                    texture,over_from_input
                )
            ) else (
                texture, over
            ) in

        (* Strectching *)
        let stretchRectOpt = make_rect 0 0 screen_width screen_height in
        let rect = manage_option stretchRectOpt "Error creating rectangle" in

        manage_result (Sdl.render_clear renderer) "Error render clear : %s";

        (* Load the renderer with the texture *)
        manage_result (
            Sdl.render_copy renderer new_texture
            ) "Error render copy : %s";

        (* Update the renderer *)
        Sdl.render_present renderer;

        (* Continue the game *)
        game renderer new_texture new_over

let machin = GameObject.create_game_object 1 2 3
let item_machin = Item.create_item 10 2 3 10 50

(* Main  *)
let () =
    let window,renderer = initialization () in
    let current_texture = load_media renderer in
    game renderer current_texture false;
    close [window] [] [renderer] [current_texture];
    Printf.printf "%d" (Item.get_x item_machin);
    ();
