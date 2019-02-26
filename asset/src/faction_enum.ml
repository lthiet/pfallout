module MFaction_enum = struct
  type code = USA | EU | ASIA | RUSSIA | NEUTRAL
  type t = code * int
  let to_string t =
    let a,b = t in
    let pre = match a with 
    | USA -> "American Megacorp."
    | EU -> "European Union Joint Task Force"
    | ASIA -> "Pacific Coast Coalition"
    | RUSSIA -> "Tsar's Army"
    | NEUTRAL -> "Neutral"
    in
    pre ^ " " ^ (string_of_int b)

  (* Faction identifier. This
  allows for multiple factions to be of the same
  code *)
  let id = ref 0

  (* Each time a faction is created, the id is incremented *)
  let create code =
    id := !id + 1;
    code,!id
end
;;