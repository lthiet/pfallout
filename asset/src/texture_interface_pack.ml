open Texture_wrapper 

module MTexture_interface_pack = struct
  type textures = {
    wstart : MTexture.t;
    wsettings : MTexture.t;
    bstart : MTexture.t;
    bsettings : MTexture.t;
  }
  
  let menu_bg_path = "asset/image/menu_bg.png" 
  let btn_path = "asset/image/btns.png" 
  let btn_settings_path = "asset/image/btn_settings.png" 
  let settings_bg_path = "asset/image/settings_bg.png"  
  let btn_return_path = "asset/image/btn_return.png" 
  let btn_Smapsize_path = "asset/image/btn_smap.png"
  let btn_Mmapsize_path = "asset/image/btn_mmap.png"
  let btn_Lmapsize_path = "asset/image/btn_lmap.png" 

  let create renderer =
    {
      wstart = MTexture.load_from_file renderer menu_bg_path;
      wsettings = MTexture.load_from_file renderer settings_bg_path;
      bstart = MTexture.load_from_file renderer btn_path;
      bsettings = MTexture.load_from_file renderer btn_settings_path;
    }

    let get_wstart t = t.wstart
    let get_wsettings t = t.wsettings
    let get_bstart t = t.bstart
    let get_bsettings t = t.bsettings

end