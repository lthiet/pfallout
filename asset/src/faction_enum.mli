module MFaction_enum :
  sig
    type code = USA | EU | ASIA | RUSSIA | NEUTRAL
    type t = code * int
    val to_string : code * int -> string
    val create : 'a -> 'a * int
  end
