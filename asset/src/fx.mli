module MFx :
  sig
    type code = HEALING | ATTACKED | NUKE_DROP
    type t = { code : code; x : int; y : int; }
    val create : code -> int -> int -> t
    val create_from_entity :
      < get_axial : Hex.MHex.axial_coord; .. > -> code -> t
    val render :
      Tsdl.Sdl.renderer ->
      t ->
      Texture_pack.MTexture_pack.textures ->
      float -> Tsdl.Sdl.rect -> int -> unit
  end
