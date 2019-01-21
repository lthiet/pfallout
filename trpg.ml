(* Modules *)
(* Utils *)
open Tsdl
open Tsdl_image
open Sdl_tools
(* Assets *)
open GameObject
open Item


(* Constants *)
let screen_width = 1920 * 2
let screen_height = 1080 * 2

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
        Sdl.Point.create x h;
        Sdl.Point.create w h;
        Sdl.Point.create w y
      ]


type pos_cursor = {
    x : int;
    y : int
}

let rec game renderer texture over pos_cursor =
    if  over then
        ()
    else
        (* Get the next event in the queue *)
        let new_over,new_pos_cursor=
            if not (Sdl.poll_event ev) then (
                match ev with
                (* If no event, nothing to do *)
                | None ->
                    over,pos_cursor
                (* Otherwise, check the event *)
                | Some e -> (
                    (* If the user clicks the red cross button, the game closes *)
                    if (Sdl.Event.get e Sdl.Event.typ) = Sdl.Event.quit then
                        true,pos_cursor
                    else if Sdl.Event.get e Sdl.Event.typ = Sdl.Event.key_down then
                            let pressed_key = Sdl.Event.get e Sdl.Event.keyboard_keycode in
                            if pressed_key = Sdl.K.escape then
                                true,pos_cursor
                            else if pressed_key = Sdl.K.down then
                                over,{pos_cursor with y = pos_cursor.y + 50}
                            else if pressed_key = Sdl.K.up then
                                over,{pos_cursor with y = pos_cursor.y - 50}
                            else if pressed_key = Sdl.K.left then
                                over,{pos_cursor with x = pos_cursor.x - 50}
                            else if pressed_key = Sdl.K.right then
                                over,{pos_cursor with x = pos_cursor.x + 50}
                            else    
                                over,pos_cursor
                    else
                        over,pos_cursor
                )
            ) else (
                over,pos_cursor
            ) in
        (* Clear *)
        manage_result (Sdl.set_render_draw_color renderer 255 255 255 255) "bla %s";
        manage_result (Sdl.render_clear renderer) "bla %s";
        
        (* Draw the new rect *)
        manage_result (Sdl_tools.draw_filled_rectangle renderer (0,0,0,255) (new_pos_cursor.y - 100 , new_pos_cursor.y + 100 ,new_pos_cursor.x - 100 ,new_pos_cursor.x + 100)) "bla %s";

        (* Update the renderer *)
        Sdl.render_present renderer;

        (* Continue the game *)
        game renderer texture new_over new_pos_cursor

let machin = GameObject.create_game_object 1 2 3
let item_machin = Item.create_item 10 2 3 10 50

(* Main  *)
let () =
    let window,renderer = initialization () in
    let current_texture = load_media renderer in
    game renderer current_texture false {
        x = screen_width / 2;
        y = screen_height / 2
    };
    close [window] [] [renderer] [current_texture];
    Printf.printf "%d" (Item.get_x item_machin);
    ();
