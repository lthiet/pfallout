open Utils
open Tsdl
open Texture_wrapper

module MMenu = struct
end;;

module MenuGraphics = struct
    let bg_t_path = "asset/image/menu_bg.png"
    let bg = ref None 

    let init renderer = 
        bg := Some (MTexture.load_from_file renderer bg_t_path)
end;;