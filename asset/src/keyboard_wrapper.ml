open Tsdl
open Utils

module MKeyboard = struct
    let get_scancode e =
        Sdl.Event.get e Sdl.Event.keyboard_scancode

    (* Compute a new coordinates,
        pk : pressed_key
        ekp : expected key positive
        ekn : expected key negative
     *)
    let new_coord pk ekp ekn offset old =
        if pk = ekp then
            old + offset
        else if pk = ekn then
            old - offset
        else
            old

    (* Return a new camera based on user input *)
    let get_camera e c =
        (* The distance at which the camera will move *)
        let offset = 10 in
        (* Check if event is a keydown *)
        if check_ev_type e Sdl.Event.key_down then
            (* If yes, check which key has been pressed *)
            let pressed_key = get_scancode e in
            let x = new_coord pressed_key Sdl.Scancode.right Sdl.Scancode.left  offset (Sdl.Rect.x c) in
            let y = new_coord pressed_key Sdl.Scancode.down Sdl.Scancode.up  offset (Sdl.Rect.y c) in
            Sdl.Rect.create x y (Sdl.Rect.w c) (Sdl.Rect.h c)
        else
            c

end
;;