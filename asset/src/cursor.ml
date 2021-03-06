open Tsdl
open Hex
open Game_object
open Texture_wrapper
open Utils
open Grid


module MCursor = struct
  type status = 
    | SELECTING
    | SELECTING_DST
    | POSSIBLE
    | IMPOSSIBLE
    | HIDDEN

  (* Match a tile type to a clip to get the texture from *)
  let match_status_to_clip t =
    let tw,th = MHex.width,MHex.height in
    let x,y,w,h = match t with
      | IMPOSSIBLE -> tw, 0,  tw,th
      | POSSIBLE -> tw*2, 0,  tw,th
      | SELECTING | SELECTING_DST | _ -> 0, 0, tw,th 
    in
    Sdl.Rect.create x y w h

  class cursor r q status = 
    object(self)
      inherit game_object r q as super
      val status : status = status
      method get_status = status
      method is_hidden = status = HIDDEN
      method is_not_hidden = not self#is_hidden
      method set_status s = {< status = s>}
    end

  let create r q status =
    new cursor r q status

  (* Functions *)
  let get_screen_x c = let x,_ = MHex.axial_to_screen_coord c#get_axial in x 
  let get_screen_y c = let _,y = MHex.axial_to_screen_coord c#get_axial in y

  (* Render a cursor *)
  let render renderer texture cursor scale camera =
    if cursor#is_not_hidden then
      MTexture.render renderer
        ~clip_src:( Some (match_status_to_clip cursor#get_status))
        ~scale:scale
        ~x:((get_screen_x cursor)- Sdl.Rect.x camera)
        ~y:((get_screen_y cursor)- Sdl.Rect.y camera)
        texture
end
;;