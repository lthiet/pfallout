open Tsdl
module MColor = struct
  let white = Sdl.Color.create 255 255 255 255
  let black = Sdl.Color.create 0 0 0 255
  let red = Sdl.Color.create 255 0 0 255
  let green = Sdl.Color.create 0 255 0 255
  let blue = Sdl.Color.create 0 0 255 255

  let to_quadruple t =
    Sdl.Color.r t,
    Sdl.Color.g t,
    Sdl.Color.b t,
    Sdl.Color.a t
end