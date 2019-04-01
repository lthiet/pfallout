(* Adapted from : https://gist.github.com/owainlewis/3829544 *)

module MTree = struct
  type 'a tree = 
      Leaf | Node of 'a * 'a tree list

  let empty = Leaf

  exception Empty_tree
  let get_father t = 
    match t with
    | Leaf -> raise Empty_tree
    | Node (a,l) -> a


  let get_sons t = 
    match t with
    | Leaf -> raise Empty_tree
    | Node (a,l) -> l

  let create x =
    Node(x,[])

  let add_sons l t =
    Node((get_father t),l)




end
