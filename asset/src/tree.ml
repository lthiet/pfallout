module MTree = struct
  type 'a tree = 
    {
      elem : 'a;
      children : 'a tree list
    }

  let create elem =
    {
      elem = elem;
      children = []
    }

  let get_elem t = t.elem
  let get_children t = t.children

  let rec iter t f =
    let () = f t.elem  in
    List.iter ( fun x -> iter x f ) t.children

  let rec fold t f acc =
    let acc_e = f acc t.elem in
    List.fold_left (fun acc_l x -> fold x f acc_l) acc_e t.children

  let rec map t f_first ff =
    let f_children = ff t.elem in
    {
      elem = f_first t.elem;
      children = List.map ( fun x -> map x f_children ff ) t.children
    }

  let is_leaf t =
    match t.children with
    | [] -> true
    | _ -> false

  let is_node t =
    not (is_leaf t)

  let append_child t x =
    let child = create x in
    {
      t with
      children = child :: t.children
    }

  let append_children t l =
    {
      t with
      children = l @ t.children
    }
end
