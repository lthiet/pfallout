module MButton :
  sig
    val width : int
    type status = IDLE | PRESSED | RELEASED
    type t = { x : int; y : int; status : status; }
    val create : int -> int -> t
    val is_released : t -> bool
    val get_coord : t -> int * int * int * int
    val render :
      Tsdl.Sdl.renderer -> float -> t -> Texture_wrapper.MTexture.t -> unit
  end
