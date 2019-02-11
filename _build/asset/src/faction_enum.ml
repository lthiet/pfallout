module MFaction_enum = struct
    type t = USA | EU | ASIA | RUSSIA | NEUTRAL
    let code_to_string c =
        match c with 
        | USA -> "American Megacorp."
        | EU -> "European Union Joint Task Force"
        | ASIA -> "Pacific Coast Coalition"
        | RUSSIA -> "Tsar's Army"
        | NEUTRAL -> "Neutral"
end
;;