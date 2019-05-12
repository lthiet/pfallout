module MInventory :
  sig
    type t = { item_list : Item.MItem.t list; }
    val to_string : t -> string
    val empty : t
    val add_item : t -> Item.MItem.t -> t
    val get_item : t -> Item.MItem.enum -> Item.MItem.t option
    val fetch_item : t -> Item.MItem.code -> Item.MItem.t option * t
  end
