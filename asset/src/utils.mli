val manage_result :
  ('a, [< `Msg of 'b ]) result ->
  ('b -> 'c, Format.formatter, unit) format -> 'a
val get_format_from_surface : Tsdl.Sdl.surface -> Tsdl.Sdl.pixel_format
val make_rect : int -> int -> int -> int -> Tsdl.Sdl.rect
val check_ev_type : Tsdl.Sdl.event -> Tsdl.Sdl.event_type -> bool
val round : float -> int
val new_int : 'a -> 'a -> 'a -> int -> int -> int
val cycle : 'a list -> 'a list
exception Empty_list
val random_elem_list : 'a list -> 'a
exception Not_yet_implemented
val incr : int ref -> int
val scale_to : int -> float -> int
val inverse : float -> float
val check_collision : Tsdl.Sdl.rect -> Tsdl.Sdl.rect -> bool
val division_eclid : int -> int -> int * int
val debug : string -> unit
