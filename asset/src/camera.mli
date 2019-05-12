module MCamera :
  sig
    type t = { rect : Tsdl.Sdl.rect; vel_x : int; vel_y : int; }
    val create : Tsdl.Sdl.rect -> t
    val get_rect : t -> Tsdl.Sdl.rect
    val update_camera : Tsdl.Sdl.window -> t -> t
    val change_direction : t -> Tsdl.Sdl.event -> t
    val scale : t -> float -> t
  end
