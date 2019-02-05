open Tsdl
open Hex
open Game_object
open Texture_wrapper
open Utils

module MCursor = struct
    type status = 
    | SELECTING
    | POSSIBLE
    | IMPOSSIBLE
    | HIDDEN

    (* Match a tile type to a clip to get the texture from *)
    let match_status_to_clip t =
        let tw,th = MHex.width,MHex.height in
        let x,y,w,h = match t with
        | IMPOSSIBLE -> 138, 0,  tw,th
        | POSSIBLE -> 276, 0,  tw,th
        | SELECTING | _ -> 0, 0, tw,th 
        in
        Sdl.Rect.create x y w h

    class cursor r q status = 
    object
        inherit game_object r q as super
        val status : status = status
        method get_status = status
        method is_hidden = status = HIDDEN
    end

    let create r q status =
        new cursor r q status

    let move cursor r q =
        let status = cursor#get_status in
        new cursor r q status

    (* Functions *)
    let get_screen_x c = let x,_ = MHex.axial_to_screen_coord c#get_axial in x 
    let get_screen_y c = let _,y = MHex.axial_to_screen_coord c#get_axial in y
   
    (* Render a tile *)
    let render renderer texture cursor camera =
        if not cursor#is_hidden then
            MTexture.render renderer
            ~clip:( Some (match_status_to_clip cursor#get_status))
            ~x:((get_screen_x cursor)- Sdl.Rect.x camera)
            ~y:((get_screen_y cursor)- Sdl.Rect.y camera)
            texture
end
;;