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
    (* Fade in *)
    let a = LTexture.load_from_file renderer "asset/image/pepe.jpg"
    in

    (* Fade out *)
    let b = LTexture.load_from_file renderer "asset/image/just.bmp"
    in
    LTexture.set_blend_mode a Sdl.Blend.mode_blend;
    a,b

type pos_cursor = {
    x : int;
    y : int
}

type rgb_offset_input = {
    r : int;
    g : int;
    b : int;
}

let rec game renderer modulated_t bg_t alpha over = 
    if over then
        ()
    else
        (* Get the new over state and the new position of the cursor *)
        let new_over,new_alpha =
            (* Get the next event in the queue *)
            if not (Sdl.poll_event ev) then
                match ev with
                (* If no event, nothing to do *)
                | None ->
                    over,alpha
                (* Otherwise, check the event *)
                | Some e ->
                    (* If the user clicks the red cross button, the game closes *)
                    if (Sdl.Event.get e Sdl.Event.typ) = Sdl.Event.quit then
                        true,alpha
                    (* Else, he has clicked a key on the keyboard *)
                    else if Sdl.Event.get e Sdl.Event.typ = Sdl.Event.key_down then

                        let pressed_key = Sdl.Event.get e Sdl.Event.keyboard_keycode in
                        let a =
                            match pressed_key with
                            | x when x = Sdl.K.up -> 
                                if alpha + 32 > 255 then
                                    255
                                else
                                    alpha + 2
                            | x when x = Sdl.K.down -> 
                                if alpha - 32 < 0 then
                                    0
                                else
                                    alpha - 2
                            | _ -> alpha
                        in
                        over,a
                    else
                        over,alpha
            else
               over,alpha
        in
        
        (* Clear *)
        manage_result (Sdl.set_render_draw_color renderer 255 255 255 255) "Error : %s";
        manage_result (Sdl.render_clear renderer) "Error : %s";
        
        (* Render the textures *)
        LTexture.render renderer None bg_t 0 0;
        LTexture.set_alpha modulated_t new_alpha;
        LTexture.render renderer None modulated_t 0 0;

        (* Update the renderer *)
        Sdl.render_present renderer;

        (* Continue the game *)
        game renderer modulated_t bg_t new_alpha new_over

let machin = GameObject.create_game_object 1 2 3
let item_machin = Item.create_item 10 2 3 10 50


(* Main  *)
let () =
    let window,renderer = initialization () in
    let modulated_t, bg_t = load_media renderer in
    game renderer modulated_t bg_t 0 false;
    close [window] [] [renderer] [] [modulated_t;bg_t];
    Printf.printf "%d" (Item.get_x item_machin);
    ();
