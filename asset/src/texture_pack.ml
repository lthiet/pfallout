open Texture_wrapper 

module MTexture_pack = struct
  type textures = {
    tile : MTexture.t;
    terrain_feature : MTexture.t;
    bg : MTexture.t;
    curs : MTexture.t;
    soldier_eu : MTexture.t;
    soldier_pac : MTexture.t;
    city : MTexture.t;
    healthpack : MTexture.t;
    nuke : MTexture.t;
    fx_healed : MTexture.t;
    fx_attacked : MTexture.t
  }

  let tile_path = "asset/image/tiles.png"
  let terrain_feature_path = "asset/image/features.png"
  let bg_path = "asset/image/bg.png"
  let cursor_path = "asset/image/cursors.png"
  let soldier_eu_path = "asset/image/soldier-eu.png"
  let soldier_pac_path = "asset/image/soldier-pac.png"
  let city_path = "asset/image/city.png"
  let healthpack_path = "asset/image/healthpack.png"
  let nuke_path = "asset/image/nuke.png"
  let fx_healed_path = "asset/image/fx_healed.png"
  let fx_attacked_path = "asset/image/fx_attacked.png"

  let create renderer = 
    {
      tile = MTexture.load_from_file renderer tile_path;
      terrain_feature = MTexture.load_from_file renderer terrain_feature_path;
      bg = MTexture.load_from_file renderer bg_path;
      curs = MTexture.load_from_file renderer cursor_path;
      soldier_eu = MTexture.load_from_file renderer soldier_eu_path;
      soldier_pac = MTexture.load_from_file renderer soldier_pac_path;
      city = MTexture.load_from_file renderer city_path;
      healthpack = MTexture.load_from_file renderer healthpack_path;
      nuke = MTexture.load_from_file renderer nuke_path;
      fx_healed = MTexture.load_from_file renderer fx_healed_path;
      fx_attacked = MTexture.load_from_file renderer fx_attacked_path;
    }

  let get_tile t = t.tile
  let get_terrain_feature t = t.terrain_feature
  let get_bg t = t.bg
  let get_curs t = t.curs
  let get_soldier_eu t = t.soldier_eu
  let get_soldier_pac t = t.soldier_pac
  let get_city t = t.city
  let get_healthpack t = t.healthpack
  let get_nuke t = t.nuke
  let get_fx_healed t = t.fx_healed
  let get_fx_attacked t = t.fx_attacked

end