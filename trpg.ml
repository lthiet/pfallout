(* Modules *)
(* Utils *)
open Tsdl
open Tsdl_image
open Tsdl_ttf
open Tsdl_mixer
open Sdl_tools
open Utils
(* Assets *)
open Texture_wrapper


(* Constants *)
let screen_width = 1920
let screen_height = 1080
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
    let a = LTexture.load_from_file renderer "asset/image/just.bmp"; in
    [|a|]

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
}

type media = {
    textures : LTexture.t array;
}

(* Provide context for game *)
let game_main renderer media param =
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
                        if check_ev_type e Sdl.Event.quit then
                            true
                        else
                            false
                else
                    false
            in


            (* Clear *)
            manage_result (Sdl.set_render_draw_color renderer 255 255 255 255) "Error : %s";
            manage_result (Sdl.render_clear renderer) "Error : %s";

            (* Update the renderer *)
            Sdl.render_present renderer;

            (* Continue the game *)
            game renderer media 
            {
                over = over
            }
    in
    game renderer media param

class oui x y =
object
    val x : int = x
    val y : int = y
    method bidule =
        x + y

    method get_x = x
    method get_y = y
end
;;

module type Oui = sig
    val truc : oui
end
;;

module Incr (M : Oui) : Oui = struct
    let truc = 
        let x = (M.truc#get_x) + 1 in
        let y = (M.truc#get_y) + 1 in
        new oui x y
end
;;

module OuiOui = struct
    let truc = new oui 0 0
end
;;

module OuiOuiOui = Incr(OuiOui)
module OuiOuiOuiOui = Incr(OuiOuiOui)

(* Main  *)
let () =
    let window,renderer = initialization () in
    let textures = load_media renderer in


    game_main renderer
    {
        textures = textures;
    } 
    {
        over = false;
    };
    close [window] [] [renderer] [] [] [||] [||];
    ();
    Printf.printf "%d" OuiOui.truc#bidule;
    Printf.printf "%d" OuiOuiOui.truc#bidule;
    Printf.printf "%d" OuiOuiOuiOui.truc#bidule;
