open Entity
open Military
open Infrastructure
open Faction_enum

module MFaction =
struct

    type t = {
        code : MFaction_enum.t;
        military_list : MMilitary.t list;
        infrastructure_list : MInfrastructure.t list;
        controlled_by_player : bool
    }

    let create_faction code cbp =
        {
            code = code;
            military_list = [];
            infrastructure_list = [];
            controlled_by_player = cbp
        }

    let add_military t m =
        {
            t with
            military_list = m :: t.military_list
        }

    let get_military t = t.military_list

    let add_infrastructure t i =
    {
        t with
        infrastructure_list = i :: t.infrastructure_list
    }

    let get_infrastructure t = t.infrastructure_list
end
;;