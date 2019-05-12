module MEntity_enum :
  sig
    type t = SOLDIER | SNIPER | CITY | FORGERY | FX_BINDER
    val max_hp_of_soldier : int
    val max_mp_of_soldier : int
    val max_hp_of_city : int
    val max_hp_of : t -> int
    val max_mp_of : t -> int
  end
