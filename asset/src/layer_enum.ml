(* The layer of the maps, currently there is infrastructure and military.
each entities of different layer can overlap themselves *)

module MLayer_enum = struct
    type t = MILITARY | INFRASTRUCTURE

end