open Tsdl
open Tsdl_image
open Utils

module LTexture = struct
    type t = {
        mTexture : Sdl.texture;
        mWidth : int;
        mHeight : int
    }

    let load_from_file renderer path =
        (* Load the image as a surface *)
        let loaded_surface = manage_result (
            Image.load path
        ) "Error load from file %s : "
        in

        let rgb_int = Sdl.map_rgb (
            get_format_from_surface loaded_surface
        ) 0 255 255
        in

        (* Color key image *)
        manage_result (
            Sdl.set_color_key loaded_surface true rgb_int
        ) "Error set color keys %s : ";

        let new_texture = manage_result (
            Sdl.create_texture_from_surface renderer loaded_surface
        ) "Error surface to texture %s";
        in

        let width,height = Sdl.get_surface_size loaded_surface 
        in

        {
            mTexture =  new_texture;
            mWidth = width;
            mHeight = height
        }

    let free t =
        Sdl.destroy_texture t.mTexture

    (* Render the texture t with renderer at position x and y on the rectangle clip *)
    let render renderer clip t x y =
        (* Get the width and height of the portion of the texture if it exists *)
        let w,h =
            match clip with
            | None ->
                t.mWidth, t.mHeight
            | Some r ->
                (Sdl.Rect.w r),(Sdl.Rect.h r)
        in


        (* Set rendering space and render to screen *)
        let renderQuad = make_rect x y w h in

        (* Render *)
        match clip with 
        | None ->
            manage_result (
                Sdl.render_copy renderer t.mTexture ~dst:renderQuad 
            ) "Error render copy : %s"
        | Some r ->
            manage_result (
                Sdl.render_copy renderer t.mTexture ~dst:renderQuad ~src:r
            ) "Error render copy : %s"

    let set_color r g b t =
        manage_result (
            Sdl.set_texture_color_mod t.mTexture r g b
        ) "Error set color mod %s"


    let set_blend_mode t m =
        manage_result (
            Sdl.set_texture_blend_mode t.mTexture m
        ) "Error blend mode %s"

    let set_alpha t a =
        manage_result (
            Sdl.set_texture_alpha_mod t.mTexture a
        ) "Error set alpha %s"
end