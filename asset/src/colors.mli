module MColor :
  sig
    val white : Tsdl.Sdl.color
    val black : Tsdl.Sdl.color
    val red : Tsdl.Sdl.color
    val green : Tsdl.Sdl.color
    val blue : Tsdl.Sdl.color
    val to_quadruple :
      Tsdl.Sdl.color ->
      Tsdl.Sdl.uint8 * Tsdl.Sdl.uint8 * Tsdl.Sdl.uint8 * Tsdl.Sdl.uint8
  end
