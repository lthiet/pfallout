(* Implement an analog to the event listener like in javascript *)
open Tsdl
open Utils

module MEvent_listener = struct

  type output =
      WINDOW_RESIZE of int * int

  type param =
      WINDOW_RESIZE of int * int


  type t = 
    {
      event : Sdl.event_type;
      func : Sdl.event -> output
    }

  let create event func =
    {
      event = event;
      func = func;
    }

  let get_event t = t.event
  let get_func t = t.func

  let compute_event t event = 
    if check_ev_type event t.event then
      Some (t.func event)
    else
      None
end