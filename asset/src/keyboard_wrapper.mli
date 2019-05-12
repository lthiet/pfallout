module MKeyboard :
  sig
    val get_scancode : Tsdl.Sdl.event -> Tsdl.Sdl.scancode
    val check_ev_key_repeat : Tsdl.Sdl.event -> bool
    val key_is_pressed : Tsdl.Sdl.event -> Tsdl.Sdl.scancode -> bool
  end
