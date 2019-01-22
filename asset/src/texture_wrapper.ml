open Tsdl
open Tsdl_image
open Utils

module LTexture = struct
    type lTexture = {
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

    let render renderer t x y =
        (* Set rendering space and render to screen *)
        let renderQuad = make_rect x y t.mWidth t.mHeight in
        manage_result (
            Sdl.render_copy renderer t.mTexture ~dst:renderQuad
        ) "Error render copy : %s"
end