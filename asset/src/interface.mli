module MInterface :
sig
  type kind = SIMPLE | COMPOSED
  type role = BUTTON | WINDOW
  type interface = {
    id : int;
    x : int;
    y : int;
    w : int option;
    h : int option;
    w_percent : float option;
    h_percent : float option;
    centered : bool;
    text : string option;
    kind : kind;
    role : role;
    clicked_tags : string list;
  }
  type interaction = {
    resize_window : (int * int) option;
    close_window : int list;
    move_window : (int * int) option;
    handlers : handler list;
    clicked_tags : string list;
  }
  and handler = Tsdl.Sdl.event -> interface -> interaction

  val get_resize_window : interaction -> (int * int) option
  val get_close_window : interaction -> int list
  val get_move_window : interaction -> (int * int) option
  val get_added_handlers : interaction -> handler list
  val add_interaction : interaction list -> interaction
  val set_x : interface -> int -> interface
  val set_y : interface -> int -> interface
  val set_w : interface -> int option -> interface
  val set_h : interface -> int option -> interface
  val get_rect : interface -> Tsdl.Sdl.rect
  type t = { interface : interface; handlers : handler list; }
  val get_interface : t -> interface
  val get_handlers : t -> handler list
  val set_interface : t -> interface -> t
  val set_handlers : t -> handler list -> t
  val incr_w : interface -> int -> interface
  val incr_h : interface -> int -> interface
  val incr_x : interface -> int -> interface
  val incr_y : interface -> int -> interface
  val create_button :
    int ->
    int option ->
    int option ->
    float option -> float option -> string -> string list -> t
  val create_window :
    int ->
    int ->
    int option -> int option -> float option -> float option -> bool -> t
  val fetch_interaction : t -> Tsdl.Sdl.event -> interaction
  val center : int -> int -> int
  val render :
    Tsdl.Sdl.renderer ->
    interface -> Texture_pack.MTexture_pack.textures -> unit
  type structure = t Tree.MTree.tree list
  val render_struct :
    Tsdl.Sdl.renderer ->
    t Tree.MTree.tree list -> Texture_pack.MTexture_pack.textures -> unit
  val post_processed_interface :
    t Tree.MTree.tree list -> t Tree.MTree.tree list
end
