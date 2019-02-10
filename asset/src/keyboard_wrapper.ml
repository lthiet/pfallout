open Tsdl
open Utils

module MKeyboard = struct
    let get_scancode e =
        Sdl.Event.get e Sdl.Event.keyboard_scancode

    let check_ev_key_repeat e =
        Sdl.Event.get e Sdl.Event.keyboard_repeat = 0
   end
;;