module MTexture_pack :
  sig
    type textures = {
      tile : Texture_wrapper.MTexture.t;
      terrain_feature : Texture_wrapper.MTexture.t;
      bg : Texture_wrapper.MTexture.t;
      curs : Texture_wrapper.MTexture.t;
      soldier_eu : Texture_wrapper.MTexture.t;
      soldier_pac : Texture_wrapper.MTexture.t;
      soldier_us : Texture_wrapper.MTexture.t;
      city : Texture_wrapper.MTexture.t;
      healthpack : Texture_wrapper.MTexture.t;
      nuke : Texture_wrapper.MTexture.t;
      fx_healed : Texture_wrapper.MTexture.t;
      fx_attacked : Texture_wrapper.MTexture.t;
      fx_nuke_drop : Texture_wrapper.MTexture.t;
      ui : Texture_wrapper.MTexture.t;
      ui_button : Texture_wrapper.MTexture.t;
    }
    val create : Tsdl.Sdl.renderer -> textures
    val get_tile : textures -> Texture_wrapper.MTexture.t
    val get_terrain_feature : textures -> Texture_wrapper.MTexture.t
    val get_bg : textures -> Texture_wrapper.MTexture.t
    val get_curs : textures -> Texture_wrapper.MTexture.t
    val get_soldier_eu : textures -> Texture_wrapper.MTexture.t
    val get_soldier_pac : textures -> Texture_wrapper.MTexture.t
    val get_soldier_us : textures -> Texture_wrapper.MTexture.t
    val get_city : textures -> Texture_wrapper.MTexture.t
    val get_healthpack : textures -> Texture_wrapper.MTexture.t
    val get_nuke : textures -> Texture_wrapper.MTexture.t
    val get_fx_healed : textures -> Texture_wrapper.MTexture.t
    val get_fx_attacked : textures -> Texture_wrapper.MTexture.t
    val get_fx_nuke_drop : textures -> Texture_wrapper.MTexture.t
    val get_ui : textures -> Texture_wrapper.MTexture.t
    val get_ui_button : textures -> Texture_wrapper.MTexture.t
  end
