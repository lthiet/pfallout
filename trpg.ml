(* Modules *)
(* Utils *)
open Tsdl
open Tsdl_image
open Sdl_tools
open Utils
(* Assets *)
open GameObject
open Texture_wrapper
open Item


(* Constants *)
let screen_width = 640
let screen_height = 480 

(* Variables *)
(* Events *)
let ev = Some (Sdl.Event.create ())

(* Types *)

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
let close windows surfaces renderers textures lTextures =
    List.iter ( fun x -> LTexture.free x) lTextures;
    List.iter ( fun x -> Sdl.destroy_window x ) windows;
    List.iter ( fun x -> Sdl.free_surface x ) surfaces;
    List.iter ( fun x -> Sdl.destroy_renderer x ) renderers;
    List.iter ( fun x -> Sdl.destroy_texture x ) textures;
    Image.quit ();
    Sdl.quit ()

(* Load all the images related to the game *)
let load_media renderer =
    let a = LTexture.load_from_file renderer "asset/image/foo.png"
    in

    let b = LTexture.load_from_file renderer "asset/image/background.png"
    in
    a,b

type pos_cursor = {
    x : int;
    y : int
}

let rec game renderer foo_texture bg_texture over pos_cursor =
    if  over then
        ()
    else
        (* Get the new over state and the new position of the cursor *)
        let new_over,new_pos_cursor=
            (* Get the next event in the queue *)
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
                    (* Else, he has clicked a key on the keyboard *)
                    else if Sdl.Event.get e Sdl.Event.typ = Sdl.Event.key_down then

                            (* Check which key it is *)
                            let offset = 10 in
                            let pressed_key = Sdl.Event.get e Sdl.Event.keyboard_keycode in
                            if pressed_key = Sdl.K.escape then
                                true,pos_cursor
                            else if pressed_key = Sdl.K.down then
                                over,{pos_cursor with y = pos_cursor.y + offset}
                            else if pressed_key = Sdl.K.up then
                                over,{pos_cursor with y = pos_cursor.y - offset}
                            else if pressed_key = Sdl.K.left then
                                over,{pos_cursor with x = pos_cursor.x - offset}
                            else if pressed_key = Sdl.K.right then
                                over,{pos_cursor with x = pos_cursor.x + offset}
                            else    
                                over,pos_cursor
                    else
                        over,pos_cursor
                )
            ) else (
                over,pos_cursor
            ) in
        
        (* Clear *)
        manage_result (Sdl.set_render_draw_color renderer 255 255 255 255) "Error : %s";
        manage_result (Sdl.render_clear renderer) "Error : %s";
        
        (* Render the textures *)
        LTexture.render renderer bg_texture 0 0;
        LTexture.render renderer foo_texture new_pos_cursor.x new_pos_cursor.y;
        (* LTexture.render renderer foo_texture 240 190; *)

        (* Update the renderer *)
        Sdl.render_present renderer;

        (* Continue the game *)
        game renderer foo_texture bg_texture new_over new_pos_cursor

let machin = GameObject.create_game_object 1 2 3
let item_machin = Item.create_item 10 2 3 10 50


(* Main  *)
let () =
    let window,renderer = initialization () in
    let foo_texture, bg_texture = load_media renderer in

    game renderer foo_texture bg_texture false {
        x = screen_width / 2;
        y = screen_height / 2
    };
    close [window] [] [renderer] [] [foo_texture;bg_texture];
    Printf.printf "%d" (Item.get_x item_machin);
    ();
