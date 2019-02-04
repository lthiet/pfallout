open Tsdl
open Utils

module MMouse = struct
    let is_inside e x y w h  =
        if 
        check_ev_type e Sdl.Event.mouse_button_down
        || check_ev_type e Sdl.Event.mouse_button_up
        || check_ev_type e Sdl.Event.mouse_motion
        then
            let _,(mx,my) = Sdl.get_mouse_state () in
            if mx < x || mx > x + w || my < y || my > y + h then
                false
            else
                true
        else
            false
end
;;