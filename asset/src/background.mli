module MBackground :
  sig
    val render :
      Tsdl.Sdl.renderer ->
      Texture_wrapper.MTexture.t -> float -> Tsdl.Sdl.rect -> unit
  end
