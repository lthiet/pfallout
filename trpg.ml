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
let walking_anim_frame = 4

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

(* Load all the images related to the game and returns an array *)
let load_media renderer =
    let a = LTexture.load_from_file renderer "asset/image/pepe.jpg"
    in
    LTexture.set_blend_mode a Sdl.Blend.mode_blend;

    let b = LTexture.load_from_file renderer "asset/image/just.bmp"
    in

    let c = LTexture.load_from_file renderer "asset/image/foo_animated.png"
    in
    [|a;b;c|]

type pos_cursor = {
    x : int;
    y : int
}

type rgb_offset_input = {
    r : int;
    g : int;
    b : int;
}

let rec game renderer t r frame alpha over coord = 
    if over then
        ()
    else
        (* Get the new over state and the new position of the cursor *)
        let new_over,new_alpha,new_coord =
            (* Get the next event in the queue *)
            if not (Sdl.poll_event ev) then
                match ev with
                (* If no event, nothing to do *)
                | None ->
                    over,alpha,coord
                (* Otherwise, check the event *)
                | Some e ->
                    (* If the user clicks the red cross button, the game closes *)
                    if (Sdl.Event.get e Sdl.Event.typ) = Sdl.Event.quit then
                        true,alpha,coord
                    (* Else, he has clicked a key on the keyboard *)
                    else if Sdl.Event.get e Sdl.Event.typ = Sdl.Event.key_down then

                        let pressed_key = Sdl.Event.get e Sdl.Event.keyboard_keycode in
                        let offset = 16 in
                        let a =
                            match pressed_key with
                            | x when x = Sdl.K.f1 -> 
                                if alpha + offset > 255 then
                                    255
                                else
                                    alpha + offset
                            | x when x = Sdl.K.f2 -> 
                                if alpha - offset < 0 then
                                    0
                                else
                                    alpha - offset
                            | _ -> alpha
                        in
                        let c =
                            match pressed_key with
                            | x when x = Sdl.K.down ->
                                {coord with y = coord.y + offset}
                            | x when x = Sdl.K.up ->
                                {coord with y = coord.y - offset}
                            | x when x = Sdl.K.right ->
                                {coord with x = coord.x + offset}
                            | x when x = Sdl.K.left ->
                                {coord with x = coord.x - offset}
                            | _ -> coord
                        in
                        over,a,c
                    else
                        over,alpha,coord
            else
               over,alpha,coord
        in
        
        (* Clear *)
        manage_result (Sdl.set_render_draw_color renderer 255 255 255 255) "Error : %s";
        manage_result (Sdl.render_clear renderer) "Error : %s";
        
        (* Render the textures *)
        LTexture.render renderer None t.(1) 0 0;
        LTexture.set_alpha t.(0) new_alpha;
        LTexture.render renderer None t.(0) new_coord.x new_coord.y;

        (* Render the animated figure *)
        let clip = r.(frame / 4)
        in

        LTexture.render renderer (Some clip) t.(2) (
            (screen_width - Sdl.Rect.w clip)/2
        ) (
            (screen_height - Sdl.Rect.h clip)/2
        );

        (* Update the renderer *)
        Sdl.render_present renderer;

        (* Continue the game *)
        let new_frame =
            let incr = frame + 1 in
            if incr / 4 >= walking_anim_frame then
                0
            else
                incr
        in
        Printf.printf "%d" new_frame;
        game renderer t r new_frame new_alpha new_over new_coord

let machin = GameObject.create_game_object 1 2 3
let item_machin = Item.create_item 10 2 3 10 50


(* Main  *)
let () =
    let window,renderer = initialization () in
    let t = load_media renderer in
    let r = [|
        make_rect 0 0 64 205;
        make_rect 64 0 64 205;
        make_rect 128 0 64 205;
        make_rect 196 0 64 205;
    |] in

    game renderer t r 0 0 false {
        x = 0;
        y = 0;
    };
    close [window] [] [renderer] [] [];
    Printf.printf "%d" (Item.get_x item_machin);
    ();
