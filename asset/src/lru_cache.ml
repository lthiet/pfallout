open Texture_wrapper


module LRUCache = struct
    exception No_key

    let cache_size = 30
    type cache_el = {
                    texture : MTexture.t;
                    timer : int 
                    }

    module Lru_cache = Map.Make(String)
                    (* String = clé ou données ?*)
                    (* Map.make(MTexture.t) FAUX? *)

    (* Creates an empty lru_cache *)
    let create_cache = Lru_cache.empty

    (* Removes an element of a given key from the Map *)
    let remove_element key =
        Lru_cache.remove key

    (* Adds an element to the Lru_cache *)
    (* If the size exceeds the authorised size, remove the less recently used *)
    (* element from the cache *)
    let add_element key el cache =
        let c_el = {texture = el; timer = 0} in      
        (* if the map's size exceeds the authorised size *)
        if ( (Lru_cache.cardinal cache)>=cache_size )
            then begin
                (* Choose a random element from the cache *)
                let (random_key,random_el) = Lru_cache.choose cache in
                (* The less recently used element's key *)
                let lru_key = Lru_cache.fold
                            (fun key c_el lru_key ->
                            let lru_el = Lru_cache.find lru_key cache in
                            if (lru_el.timer < c_el.timer)
                                then key
                            else 
                                lru_key) cache random_key 
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
                            Lru_cache.add key { texture = c_el.texture ; timer = (c_el.timer+1) } updated_cache )
                            cache create_cache 
        in
        updated_cache
    
    (* Gets an element from the map, sets the timer of the element back to 0 *)
    (* Returns the element and the updated map *)
    let get_element key cache (f:(string -> MTexture.t)) = 
        (* Retrieve the element of given key *)
        let c_el = 
            try Lru_cache.find key cache
            with Not_found -> 
                begin
                let elem = f key in
                {texture = elem ; timer = 0}
                end
        in
        (* Update the cache *)
        let updated_cache = Lru_cache.add key {texture = c_el.texture ; timer = 0} cache
        in
        c_el.texture, updated_cache
        
end