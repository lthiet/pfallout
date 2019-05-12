module MMenu :
  sig
    type context = {
      over : bool;
      btn_start : Button.MButton.t;
      settings : bool;
      btn_settings : Button.MButton.t;
      map_size : int;
      btn_return : Button.MButton.t;
      btn_Smapsize : Button.MButton.t;
      btn_Mmapsize : Button.MButton.t;
      btn_Lmapsize : Button.MButton.t;
      window : Tsdl.Sdl.window;
    }
    type textures = {
      bg : Texture_wrapper.MTexture.t;
      btn : Texture_wrapper.MTexture.t;
      btn_settings : Texture_wrapper.MTexture.t;
      settings_bg : Texture_wrapper.MTexture.t;
      btn_return : Texture_wrapper.MTexture.t;
      btn_Smapsize : Texture_wrapper.MTexture.t;
      btn_Mmapsize : Texture_wrapper.MTexture.t;
      btn_Lmapsize : Texture_wrapper.MTexture.t;
    }
    type result = {
      start_game : bool;
      map_size : int;
      window : Tsdl.Sdl.window;
    }
    val get_window : result -> Tsdl.Sdl.window
    val get_map_size : result -> int
    val run : Tsdl.Sdl.renderer -> Tsdl.Sdl.window -> result
  end
