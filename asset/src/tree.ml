module MTree = struct
  type 'a tree = 
    {
      elem : 'a;
      children : 'a tree list
    }

  let get_elem t = t.elem
  let get_children t = t.children

  let rec iter t f =
    let () = f t.elem in
    List.iter ( fun x -> iter x f ) t.children

  let rec fold t f acc =
    let acc_e = f acc t.elem in
    List.fold_left (fun acc_l x -> fold x f acc_l) acc_e t.children

end
