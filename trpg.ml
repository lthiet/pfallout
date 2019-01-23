(* Modules *)
(* Utils *)
open Tsdl
open Tsdl_image
open Tsdl_ttf
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
    (* Init the true text font *)
    manage_result (Ttf.init ()) "Error ttf init %s";
    if not (Image.Init.eq png_load_flags png_init) then (
        Sdl.log "Error loader png or ttf"; exit 1
    )
    else
        window,renderer

(* safely close all the windows and renders *)
let close windows surfaces renderers textures lTextures  =
    List.iter ( fun x -> LTexture.free x) lTextures;
    List.iter ( fun x -> Sdl.destroy_window x ) windows;
    List.iter ( fun x -> Sdl.free_surface x ) surfaces;
    List.iter ( fun x -> Sdl.destroy_renderer x ) renderers;
    List.iter ( fun x -> Sdl.destroy_texture x ) textures;
    Image.quit ();
    Sdl.quit ();
    Ttf.quit ()

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

let rec game renderer t r param = 
    if param.over then
        ()
    else
        (* Get the new over state and the new position of the cursor *)
        let new_param =
            (* Get the next event in the queue *)
            if not (Sdl.poll_event ev) then
                match ev with
                (* If no event, nothing to do *)
                | None ->
                    param
                (* Otherwise, check the event *)
                | Some e ->
                    (* If the user clicks the red cross button, the game closes *)
                    if (Sdl.Event.get e Sdl.Event.typ) = Sdl.Event.quit then
                        {param with over = true}
                    (* Else, he has clicked a key on the keyboard *)
                    else if Sdl.Event.get e Sdl.Event.typ = Sdl.Event.key_down then

                        (* Get the pressed key *)
                        let pressed_key = Sdl.Event.get e Sdl.Event.keyboard_keycode in

                        (* Allow the change of the level of transparency for some texture *)
                        let alpha =
                            let offset = 16 in
                            let old_alpha = param.alpha in
                            match pressed_key with
                            | x when x = Sdl.K.f1 -> 
                                if old_alpha + offset > 255 then
                                    255
                                else
                                    old_alpha + offset
                            | x when x = Sdl.K.f2 -> 
                                if old_alpha - offset < 0 then
                                    0
                                else
                                    old_alpha - offset
                            | _ -> old_alpha
                        in

                        (* Allow input to modify X/Y coord. for some texture *)
                        let coord =
                            let old_coord = param.coord in
                            let offset = 20 in
                            match pressed_key with
                            | x when x = Sdl.K.down ->
                                {old_coord with y = old_coord.y + offset}
                            | x when x = Sdl.K.up ->
                                {old_coord with y = old_coord.y - offset}
                            | x when x = Sdl.K.right ->
                                {old_coord with x = old_coord.x + offset}
                            | x when x = Sdl.K.left ->
                                {old_coord with x = old_coord.x - offset}
                            | _ -> old_coord
                        in

                        (* Allow the change of angle of rotation *)
                        let angle =
                            let old_angle = param.angle in
                            let offset = 20. in
                            match pressed_key with
                            | x when x = Sdl.K.return ->
                                old_angle +. offset
                            | x when x = Sdl.K.rshift ->
                                old_angle -. offset
                            | _ -> old_angle
                        in
                        let flip =
                            let old_flip = param.flip in
                            match pressed_key with
                            | x when x = Sdl.K.f12 ->
                                Sdl.Flip.none
                            | x when x = Sdl.K.f11 ->
                                Sdl.Flip.horizontal
                            | x when x = Sdl.K.f10 ->
                                Sdl.Flip.vertical
                            | _ -> old_flip
                        in

                        (* Return the new parameters *)
                        {param with alpha = alpha;coord = coord;angle = angle;flip = flip}

                    else
                        param
            else
                param
        in
        
        (* Get the newly computed params *)
        let alpha = new_param.alpha in
        let coord = new_param.coord in
        let angle = new_param.angle in
        let flip = new_param.flip in
        
        (* Clear *)
        manage_result (Sdl.set_render_draw_color renderer 255 255 255 255) "Error : %s";
        manage_result (Sdl.render_clear renderer) "Error : %s";
        
        (* Render the textures *)
        LTexture.render renderer t.(1);
        LTexture.set_alpha t.(0) alpha;
        LTexture.render renderer ~x:coord.x ~y:coord.y t.(0);

        (* Render the animated figure *)
        let clip = r.(param.frame / 4)
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
        game renderer t r {new_param with frame = frame}

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

    game renderer t r {
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
    close [window] [] [renderer] [] [];
    ();
