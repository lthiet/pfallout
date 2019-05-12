module MPriority_queue :
  sig
    type priority = int
    type 'a queue = Empty | Node of priority * 'a * 'a queue * 'a queue
    val empty : 'a queue
    val insert : 'a queue -> priority -> 'a -> 'a queue
    val extract : 'a queue -> priority * 'a * 'a queue
    val is_empty : 'a queue -> bool
  end
