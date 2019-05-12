module MTree :
  sig
    type 'a tree = { elem : 'a; children : 'a tree list; }
    val create : 'a -> 'a tree
    val get_elem : 'a tree -> 'a
    val get_children : 'a tree -> 'a tree list
    val iter : 'a tree -> ('a -> unit) -> unit
    val fold : 'a tree -> ('b -> 'a -> 'b) -> 'b -> 'b
    val map : 'a tree -> ('a -> 'b) -> ('a -> 'a -> 'b) -> 'b tree
    val append_child : 'a tree -> 'a -> 'a tree
    val append_children : 'a tree -> 'a tree list -> 'a tree
  end
