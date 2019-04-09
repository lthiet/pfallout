(* Implement an analog to the event listener like in javascript *)
open Tsdl
open Utils
open Interface

module MEvent_listener = struct

  type output =
      WINDOW_RESIZE_O of MInterface.t

  type param =
      WINDOW_RESIZE_P 

  let same_output_param output param =
    match output,param with
    | WINDOW_RESIZE_O _ ,WINDOW_RESIZE_P -> true

  let id = ref 0

  type t = 
    {
      id : int;
      event : Sdl.event_type;
      interface : MInterface.t;
      func : Sdl.event -> t -> param -> output 
    }

  let create event interface func =
    {
      id = incr id;
      event = event;
      interface : MInterface.t;
      func = func;
    }

  let get_id t = t.id
  let get_event t = t.event
  let get_interface t = t.interface
  let get_func t = t.func

  let set_interface t i = 
    {
      t with
      interface = i
    }


  let compute_event t event param = 
    if check_ev_type event t.event then
      Some (t.func event t param )
    else
      None
end