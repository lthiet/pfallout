open Texture_wrapper
open Utils

module MLRUCache = struct
  exception No_key

  let cache_size = 10
  type cache_el = {
    texture : MTexture.t;
    timer : int ;
    rm_f : (MTexture.t -> unit) option
  }

  module Lru_cache = Map.Make(String)

  type t = cache_el Lru_cache.t

  (* Creates an empty lru_cache *)
  let create_cache = Lru_cache.empty

  (* Removes an element of a given key from the Map *)
  let remove_element key =
    Lru_cache.remove key

  (* Adds an element to the Lru_cache *)
  (* If the size exceeds the authorised size, remove the less recently used *)
  (* element from the cache *)
  let add_element key el rm_f cache =
    let c_el = {texture = el; timer = 0; rm_f = Some rm_f} in      
    (* if the map's size exceeds the authorised size *)
    if ( (Lru_cache.cardinal cache)>=cache_size )
    then
      begin
        (* Choose a random element from the cache *)
        let (random_key,random_el) = Lru_cache.choose cache in
        (* The less recently used element's key *)
        let lru_key = Lru_cache.fold
            (fun key c_el lru_key ->
               let lru_el = Lru_cache.find lru_key cache in
               if (lru_el.timer > c_el.timer)
               then key
               else 
                 lru_key) cache random_key 
        in
        (* Free the memory *)
        let () = 
          let elem = Lru_cache.find lru_key cache in
          match elem.rm_f with
          | None -> ()
          | Some f -> f elem.texture
        in
        let updated_cache = remove_element lru_key cache in
        Lru_cache.add key c_el updated_cache
      end
    else 
      Lru_cache.add key c_el cache

  (* Increments de timer of all elements *)
  let increment_timer (cache : (cache_el) Lru_cache.t) =
    let updated_cache = Lru_cache.fold 
        (fun key (c_el : cache_el) updated_cache -> 
           Lru_cache.add key { c_el with timer = (c_el.timer+1) } updated_cache )
        cache create_cache 
    in
    updated_cache

  (* Gets an element from the map, sets the timer of the element back to 0 *)
  (* Returns the element and the updated map *)
  let get_element key cache (f:(string -> MTexture.t)) (rm_f:(MTexture.t -> unit)) = 
    (* Retrieve the element of given key *)
    let c_el = 
      try Lru_cache.find key cache
      with Not_found -> 
        begin
          let elem = f key in
          {texture = elem ; timer = 0;rm_f = Some rm_f}
        end
    in
    (* Update the cache *)
    let updated_cache = increment_timer (Lru_cache.add key {texture = c_el.texture ; timer = 0;rm_f = Some rm_f} cache)
    in
    c_el.texture, updated_cache

end