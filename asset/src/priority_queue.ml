(* from http://caml.inria.fr/pub/docs/manual-ocaml/moduleexamples.html as of 8/2/2019 *)

module MPriority_queue = struct
  type priority = int
  type 'a queue = Empty | Node of priority * 'a * 'a queue * 'a queue
  let empty = Empty
  let rec insert queue prio elt =
    match queue with
      Empty -> Node(prio, elt, Empty, Empty)
    | Node(p, e, left, right) ->
      if prio <= p
      then Node(prio, elt, insert right p e, left)
      else Node(p, e, insert right prio elt, left)
  exception Queue_is_empty
  let rec remove_top = function
      Empty -> raise Queue_is_empty
    | Node(prio, elt, left, Empty) -> left
    | Node(prio, elt, Empty, right) -> right
    | Node(prio, elt, (Node(lprio, lelt, _, _) as left),
           (Node(rprio, relt, _, _) as right)) ->
      if lprio <= rprio
      then Node(lprio, lelt, remove_top left, right)
      else Node(rprio, relt, left, remove_top right)
  let extract = function
      Empty -> raise Queue_is_empty
    | Node(prio, elt, _, _) as queue -> (prio, elt, remove_top queue)

  let is_empty q =
    match q with
    | Empty -> true
    | _ -> false

  let rec exists f q =
    match q with
    | Empty -> false
    | Node (_,a,l,r) ->
      (f a) || exists f l || exists f r

  let rec size f =
    match f with
    | Empty -> 0
    | Node (_,a,l,r) -> 1 + size l + size r

end
;;