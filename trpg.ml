(* Modules *)
(* Utils *)
open Tsdl
open Tsdl_image
open Tsdl_ttf
open Tsdl_mixer
open Sdl_tools
open Utils
(* Assets *)
open GameObject
open Texture_wrapper
open Item
open Button


(* Constants *)
let screen_width = 640
let screen_height = 480 
let walking_anim_frame = 4

(* Variables *)
(* Events *)
let ev = Some (Sdl.Event.create ())

(* Types *)

(* Functions *)
(* Initialize a window and a renderer *)
let initialization () = 
    (* Initialize SDL *)
    let init_flag = Sdl.Init.(+) Sdl.Init.video Sdl.Init.audio in
    manage_result ( Sdl.init init_flag) "Error init : %s";

    (* Open a Window *)
    let window = manage_result (Sdl.create_window "TRPG" ~w:screen_width ~h:screen_height Sdl.Window.windowed ) "Error create window : %s" in

    (* Get renderer from Window *)
    let create_renderer_flag = (Sdl.Renderer.(+)) Sdl.Renderer.accelerated Sdl.Renderer.presentvsync in 
    let renderer = manage_result (Sdl.create_renderer ~index:(-1) ~flags:create_renderer_flag window) "Error create renderer : %s" in

    (* Set the color of the renderer *)
    manage_result (Sdl.set_render_draw_color renderer 255 255 255 255) "Error set color renderer %s";

    (* Initialize the mixer *)
    manage_result (
        Mixer.open_audio 44100 Mixer.default_format 2 2048
    ) "Error init mixer %s";

    (* Load PNG Loading *)
    let png_load_flags = Image.Init.png in
    let png_init = Image.init png_load_flags in
    (* Init the true text font *)
    manage_result (Ttf.init ()) "Error ttf init %s";
    if not (Image.Init.eq png_load_flags png_init) then (
        Sdl.log "Error loader png or ttf"; exit 1
    )
    else
        window,renderer

(* safely close all the windows and renders *)
let close windows surfaces renderers textures lTextures musics sounds =
    List.iter ( fun x -> LTexture.free x) lTextures;
    List.iter ( fun x -> Sdl.destroy_window x ) windows;
    List.iter ( fun x -> Sdl.free_surface x ) surfaces;
    List.iter ( fun x -> Sdl.destroy_renderer x ) renderers;
    List.iter ( fun x -> Sdl.destroy_texture x ) textures;
    Array.iter ( fun x -> Mixer.free_music x ) musics;
    Array.iter ( fun x -> Mixer.free_chunk x ) sounds;
    Image.quit ();
    Sdl.quit ();
    Ttf.quit ();
    Mixer.quit ()

let load_font () = 
    manage_result (
        Ttf.open_font "asset/image/lazy.ttf" 28
    ) "Error loading font %s"

(* Load all the images related to the game and returns an array *)
let load_media renderer =
    let font = load_font () in
    let color = Sdl.Color.create 0 0 0 255 in
    let a = LTexture.load_from_rendered_text renderer font "Appuyez sur entree pour commencer" color in
    [|a|]

let load_clip () = 
    Array.init 4 ( fun i -> 
        Sdl.Rect.create 0 (i*200) LButton.button_width LButton.button_height
    )

let load_buttons () =
    let t = LButton.default in
    [|
        LButton.set_pos t 0 0;
        LButton.set_pos t (screen_width - LButton.button_width) 0;
        LButton.set_pos t 0 (screen_height - LButton.button_height);
        LButton.set_pos t (screen_width - LButton.button_width) (screen_height - LButton.button_height)
    |]

let load_music () =
    [||]

let load_sound () =
    [||]

type coord = {
    x : int;
    y : int
}

type param = {
    over : bool;
    start : Sdl.uint32;
}

type media = {
    font : Ttf.font;
    textures : LTexture.t array;
    color : Sdl.color
}

let check_key_scan ks i =
    (ks.{i} = 1)

let manage_key ks =
    if check_key_scan ks Sdl.Scancode.return then
        Some (Sdl.get_ticks ())
    else
        None

let rec game renderer media param = 
    if param.over then
        ()
    else
        let over =
            (* Get the next event in the queue *)
            if not (Sdl.poll_event ev) then
                 match ev with
                (* If no event, nothing to do *)
                | None ->
                    false
                (* Otherwise, check the event *)
                | Some e ->
                    (* If the user clicks the red cross button, the game closes *)
                    if (Sdl.Event.get e Sdl.Event.typ) = Sdl.Event.quit then
                        true
                    else
                        false
            else
                false
        in

        (* Get the keystate *)
        let key_state = Sdl.get_keyboard_state () in
        let start = match manage_key key_state with
        | Some x -> x
        | None -> param.start
        in 

        (* Clear *)
        manage_result (Sdl.set_render_draw_color renderer 255 255 255 255) "Error : %s";
        manage_result (Sdl.render_clear renderer) "Error : %s";

        let time_passed = (Int32.sub (Sdl.get_ticks ()) param.start) in
        let txt = "Millisecondes depuis le debut : " ^ (Int32.to_string time_passed) ^ "ms" in

        let txt_t = LTexture.load_from_rendered_text renderer media.font txt media.color in
        LTexture.render renderer 
        ~x:((screen_width - (LTexture.get_w media.textures.(0)))/2)
        media.textures.(0);

        LTexture.render renderer
        ~x:((screen_width - (LTexture.get_w txt_t))/2)
        ~y:((screen_height - (LTexture.get_h txt_t))/2)
        txt_t;


        (* Update the renderer *)
        Sdl.render_present renderer;

        (* Continue the game *)
        game renderer media 
        {
            start = start;
            over = over;
        }

let machin = GameObject.create_game_object 1 2 3
let item_machin = Item.create_item 10 2 3 10 50


(* Main  *)
let () =
    let window,renderer = initialization () in
    let textures = load_media renderer in
    let font = load_font () in
    let color = Sdl.Color.create 0 0 0 255 in


    game renderer
    {
        font = font;
        textures = textures;
        color = color
    } 
    {
        over = false;
        start = Int32.zero
    };
    close [window] [] [renderer] [] [] [||] [||];
    ();
