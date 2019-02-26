open Tsdl
open Utils

module MKeyboard = struct
  let get_scancode e =
    Sdl.Event.get e Sdl.Event.keyboard_scancode

  let check_ev_key_repeat e =
    Sdl.Event.get e Sdl.Event.keyboard_repeat = 0

  (* EPK : expected pressed key *)
  let key_is_pressed e epk =
    check_ev_type e Sdl.Event.key_down && (
      let pk = get_scancode e in
      pk = epk
    )
end
;;