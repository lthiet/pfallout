open Tsdl
open Tsdl_mixer
open Utils

module MSound = struct
  let music_path = "asset/sound/music/main.ogg"


  type pack = {
    music : Mixer.music
  }

  let create_pack () = 
    {
      music = 
        manage_result (
          Mixer.load_mus music_path
        ) "Error : %s"
    }

  let get_music pack = pack.music

  let play music =
    let _ = manage_result (
        Mixer.play_music music (-1)
      ) "Error : %s"
    in ()


end
