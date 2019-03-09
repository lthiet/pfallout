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
  }

  let create tile tf bg curs s_eu s_pac city =
    {
        tile = tile;
        terrain_feature = tf;
        bg = bg;
        curs = curs;
        soldier_eu = s_eu;
        soldier_pac = s_pac;
        city = city;
    }

  let get_tile t = t.tile
  let get_terrain_feature t = t.terrain_feature
  let get_bg t = t.bg
  let get_curs t = t.curs
  let get_soldier_eu t = t.soldier_eu
  let get_soldier_pac t = t.soldier_pac
  let get_city t = t.city


end