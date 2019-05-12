module MSound :
  sig
    type pack = { music : Tsdl_mixer.Mixer.music; }
    val create_pack : unit -> pack
    val get_music : pack -> Tsdl_mixer.Mixer.music
    val play : Tsdl_mixer.Mixer.music -> unit
  end
