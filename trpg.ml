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
    (* Small character *)
    let a = LTexture.load_from_file renderer "asset/image/foo.png"
    in

    (* Background *)
    let b = LTexture.load_from_file renderer "asset/image/background.png"
    in

    (* Some dots *)
    let c = LTexture.load_from_file renderer "asset/image/dots.png"
    in
    a,b,c

type pos_cursor = {
    x : int;
    y : int
}

type rgb_offset_input = {
    r : int;
    g : int;
    b : int;
}

let rec game renderer foo_texture bg_texture dots_texture tl tr bl br over pos_cursor rgb_offset_input =
    if  over then
        ()
    else
        (* Get the new over state and the new position of the cursor *)
        let new_over,new_pos_cursor,new_rgb_offset_input =
            (* Get the next event in the queue *)
            if not (Sdl.poll_event ev) then (
                match ev with
                (* If no event, nothing to do *)
                | None ->
                    over,pos_cursor,rgb_offset_input
                (* Otherwise, check the event *)
                | Some e -> (
                    (* If the user clicks the red cross button, the game closes *)
                    if (Sdl.Event.get e Sdl.Event.typ) = Sdl.Event.quit then
                        true,pos_cursor,rgb_offset_input
                    (* Else, he has clicked a key on the keyboard *)
                    else if Sdl.Event.get e Sdl.Event.typ = Sdl.Event.key_down then

                            (* Check which key it is *)
                            let offset = 10 in
                            let pressed_key = Sdl.Event.get e Sdl.Event.keyboard_keycode in
                            if pressed_key = Sdl.K.escape then
                                true,pos_cursor,rgb_offset_input
                            else if pressed_key = Sdl.K.down then
                                over,{pos_cursor with y = pos_cursor.y + offset},rgb_offset_input
                            else if pressed_key = Sdl.K.up then
                                over,{pos_cursor with y = pos_cursor.y - offset},rgb_offset_input
                            else if pressed_key = Sdl.K.left then
                                over,{pos_cursor with x = pos_cursor.x - offset},rgb_offset_input
                            else if pressed_key = Sdl.K.right then
                                over,{pos_cursor with x = pos_cursor.x + offset},rgb_offset_input
                            else if pressed_key = Sdl.K.f1 then
                                over,pos_cursor,{rgb_offset_input with r = rgb_offset_input.r + 2}
                            else if pressed_key = Sdl.K.f2 then
                                over,pos_cursor,{rgb_offset_input with r = rgb_offset_input.r - 2}
                            else if pressed_key = Sdl.K.f3 then
                                over,pos_cursor,{rgb_offset_input with g = rgb_offset_input.g + 2}
                            else if pressed_key = Sdl.K.f4 then
                                over,pos_cursor,{rgb_offset_input with g = rgb_offset_input.g - 2}
                            else if pressed_key = Sdl.K.f5 then
                                over,pos_cursor,{rgb_offset_input with b = rgb_offset_input.b + 2}
                            else if pressed_key = Sdl.K.f6 then
                                over,pos_cursor,{rgb_offset_input with b = rgb_offset_input.b - 2}
                            else    
                                over,pos_cursor,rgb_offset_input
                    else
                        over,pos_cursor,rgb_offset_input
                )
            ) else (
                over,pos_cursor,rgb_offset_input
            ) in
        
        (* Clear *)
        manage_result (Sdl.set_render_draw_color renderer 255 255 255 255) "Error : %s";
        manage_result (Sdl.render_clear renderer) "Error : %s";
        
        (* Render the textures *)
        LTexture.set_color 
            new_rgb_offset_input.r
            new_rgb_offset_input.g
            new_rgb_offset_input.b
            bg_texture;
        LTexture.render renderer None bg_texture 0 0;
        LTexture.render renderer None foo_texture new_pos_cursor.x new_pos_cursor.y;

        (* Render the dots *)
        LTexture.render renderer (Some tl) dots_texture 0 0;
        LTexture.render renderer (Some tr) dots_texture (screen_width - (Sdl.Rect.w tr)) 0;
        LTexture.render renderer (Some bl) dots_texture 0 (screen_height - (Sdl.Rect.h bl));
        LTexture.render renderer (Some br) dots_texture (screen_width - (Sdl.Rect.w br)) (screen_height - (Sdl.Rect.h br));

        (* Update the renderer *)
        Sdl.render_present renderer;

        (* Continue the game *)
        game renderer foo_texture bg_texture dots_texture tl tr bl br new_over new_pos_cursor new_rgb_offset_input

let machin = GameObject.create_game_object 1 2 3
let item_machin = Item.create_item 10 2 3 10 50


(* Main  *)
let () =
    let window,renderer = initialization () in
    let foo_texture, bg_texture, dots_texture = load_media renderer in

    (* rectangles to know which dots we want *)
    let tl = make_rect 0 0 100 100 in
    let tr = make_rect 100 0 100 100 in
    let bl = make_rect 0 100 100 100 in
    let br = make_rect 100 100 100 100 in

    game renderer foo_texture bg_texture dots_texture tl tr bl br false {
        x = screen_width / 2;
        y = screen_height / 2
    } {
        r = 1;
        g = 45;
        b = 199
    };
    close [window] [] [renderer] [] [foo_texture;bg_texture];
    Printf.printf "%d" (Item.get_x item_machin);
    ();
