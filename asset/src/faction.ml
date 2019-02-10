open Entity
open Military
open Infrastructure

module MFaction =
struct
    type code = USA | EU | ASIA | RUSSIA | NEUTRAL

    type t = {
        code : code;
        entity_list :  MEntity.t list
    }

    let code_to_string c =
        match c with 
        | USA -> "American Megacorp."
        | EU -> "European Union Joint Task Force"
        | ASIA -> "Pacific Coast Coalition"
        | RUSSIA -> "Tsar's Army"
        | NEUTRAL -> "Neutral"

    let get_entity_list t = t.entity_list
end
;;