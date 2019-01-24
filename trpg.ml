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
    let a = LTexture.load_from_file renderer "asset/image/pepe.jpg"
    in
    LTexture.set_blend_mode a Sdl.Blend.mode_blend;

    let b = LTexture.load_from_file renderer "asset/image/just.bmp"
    in

    let c = LTexture.load_from_file renderer "asset/image/foo_animated.png"
    in

    let font = manage_result (
        Ttf.open_font "asset/image/lazy.ttf" 28
    ) "Error font %s" in

    let color = Sdl.Color.create 0 0 0 255 in

    let d = LTexture.load_from_rendered_text renderer font "jme presenete je mappelle henri" color in

    let e = LTexture.load_from_file renderer "asset/image/button.png" in
    [|a;b;c;d;e|]

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
    let a = manage_result (
        Mixer.load_mus "asset/sound/beat.wav"
        ) "Error loading music %s";
    in
    [|a|]

let load_sound () =
    let a = manage_result (
        Mixer.load_wav "asset/sound/scratch.wav"
        ) "Error loading sound %s"
    in

    let b = manage_result (
        Mixer.load_wav "asset/sound/high.wav"
        ) "Error loading sound %s"
    in

    let c = manage_result (
        Mixer.load_wav "asset/sound/medium.wav"
        ) "Error loading sound %s"
    in

    let d = manage_result (
        Mixer.load_wav "asset/sound/low.wav"
        ) "Error loading sound %s"
    in

    [|a;b;c;d|]

type coord = {
    x : int;
    y : int
}

type param = {
    over : bool;
    frame : int;
    alpha : int;
    angle : float;
    coord: coord;
    flip: Sdl.flip
}

let rec game renderer t r m s btns param = 
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
                    (* Manage events for buttons *)
                    Array.iteri (
                        fun i x -> btns.(i) <- LButton.handle_event x e
                    ) btns;

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
        let coord = 
            let offset = 20 in
            if (key_state.{Sdl.Scancode.w} = 1) then
                {param.coord with y = param.coord.y - offset}
            else if (key_state.{Sdl.Scancode.s} = 1)  then
                {param.coord with y = param.coord.y + offset}
            else if (key_state.{Sdl.Scancode.a} = 1)  then
                {param.coord with x = param.coord.x - offset}
            else if (key_state.{Sdl.Scancode.d} = 1)  then
                {param.coord with x = param.coord.x + offset}
            else
                param.coord
        in

        (* Allow the change of the level of transparency for some texture *)
        let alpha =
            let offset = 16 in
            let old_alpha = param.alpha in
            if (key_state.{Sdl.Scancode.e} = 1) then
                if old_alpha + offset > 255 then
                    255
                else
                    old_alpha + offset
            else if (key_state.{Sdl.Scancode.q} = 1) then
                if old_alpha - offset < 0 then
                    0
                else
                    old_alpha - offset
            else
                old_alpha
        in

        (* Allow the change of angle of rotation *)
        let angle =
            let old_angle = param.angle in
            let offset = 20. in
            if (key_state.{Sdl.Scancode.o} = 1) then
                old_angle +. offset
            else if (key_state.{Sdl.Scancode.p} = 1) then
                old_angle -. offset
            else
                old_angle
        in

        let flip =
            let old_flip = param.flip in
            if (key_state.{Sdl.Scancode.j} = 1) then
                Sdl.Flip.none
            else if (key_state.{Sdl.Scancode.k} = 1) then
                Sdl.Flip.horizontal
            else if (key_state.{Sdl.Scancode.l} = 1) then
                Sdl.Flip.vertical
            else
                old_flip
        in

        let n = if (key_state.{Sdl.Scancode.c} = 1) then
            manage_result (
                Mixer.play_channel (-1) s.(0) 0
            ) "Error play channel %s"
        else if (key_state.{Sdl.Scancode.v} = 1) then
            manage_result (
                Mixer.play_channel (-1) s.(1) 0
            ) "Error play channel %s"
        else if (key_state.{Sdl.Scancode.b} = 1) then
            manage_result (
                Mixer.play_channel (-1) s.(2) 0
            ) "Error play channel %s"
        else if (key_state.{Sdl.Scancode.n} = 1) then
            manage_result (
                Mixer.play_channel (-1) s.(3) 0
            ) "Error play channel %s"
        else if (key_state.{Sdl.Scancode.x} = 1) then 
            (* No music player : we play music *)
            if not (Mixer.playing_music ()) then
                (* Play music *)
                manage_result (
                    Mixer.play_music m.(0) (-1)
                    ) "Error play music %s"

            (* The music is played *)
            else 
                let () =
                (* If the music is paused *)
                if Mixer.paused_music () then
                    Mixer.resume_music ()
                else
                    Mixer.pause_music ()
                in
                0

        else if (key_state.{Sdl.Scancode.z} = 1) then
            let () =manage_result (
                Mixer.halt_music ()
            ) "Error halt music %s"
            in
            0
        else
            0
        in


        (* Clear *)
        manage_result (Sdl.set_render_draw_color renderer 255 255 255 255) "Error : %s";
        manage_result (Sdl.render_clear renderer) "Error : %s";

        (* Render Buttons *)
        Array.iter ( 
            fun x ->
                LButton.render renderer x t.(4) r.(1)
            )
            btns;
        
        (* Render the textures *)
        LTexture.render renderer t.(1);
        LTexture.set_alpha t.(0) alpha;
        LTexture.render renderer ~x:coord.x ~y:coord.y t.(0);

        (* Render the animated figure *)
        let clip = r.(0).(param.frame / 4)
        in
        LTexture.render renderer
            ~clip:(Some clip)
            ~x:((screen_width - Sdl.Rect.w clip)/2)
            ~y:((screen_height - Sdl.Rect.h clip)/2)
            ~angle:angle
            ~flip:flip
            t.(2);

        (* Render some text *)
        LTexture.render renderer 
            ~x:((screen_width - (LTexture.get_w t.(3)))/2)
            ~y:((screen_height - (LTexture.get_h t.(3)))/2)
            t.(3);


        (* Update the renderer *)
        Sdl.render_present renderer;

        (* Continue the game *)

        (* Compute the new frame *)
        let frame =
            let incr = param.frame + 1 in
            if incr / 4 >= walking_anim_frame then
                0
            else
                incr
        in
        game renderer t r m s btns
        {
            over = over;
            frame = frame;
            alpha = alpha;
            coord = coord;
            angle = angle;
            flip = flip
        }

let machin = GameObject.create_game_object 1 2 3
let item_machin = Item.create_item 10 2 3 10 50


(* Main  *)
let () =
    let window,renderer = initialization () in
    let t = load_media renderer in
    let r1 = [|
        make_rect 0 0 64 205;
        make_rect 64 0 64 205;
        make_rect 128 0 64 205;
        make_rect 196 0 64 205;
    |] in

    let r2 = load_clip () in
    let btns = load_buttons () in
    let musics = load_music () in
    let sounds = load_sound () in

    game renderer t [|r1;r2|] musics sounds btns
    {
        over = false;
        frame = 0;
        alpha = 0;
        angle = 0.;
        flip = Sdl.Flip.none;
        coord = {
            x = 0;
            y = 0
        }
    };
    close [window] [] [renderer] [] [] musics sounds;
    ();
