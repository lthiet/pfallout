(* Modules *)
open Tsdl

(* Constants *)
let screen_width = 640
let screen_height = 480
let bg_path = "./asset/image/just.bmp"


(* Utils function *)
(* Manage Result *)
let manage_result r =
    match r with
    | Ok a -> a
    | Error (`Msg e) -> Sdl.log "Error : %s" e;exit 1

exception NonePointer

let deref_option p =
    match !p with
    | Some x -> x
    | None -> raise NonePointer
(* Functions *)


(* Initialize a window and a surface *)
let initialization window_p surface_p = 
    (* Initialize SDL *)
    manage_result ( Sdl.init Sdl.Init.everything );

    (* Open a Window *)
    window_p := Some ( manage_result (Sdl.create_window "TRPG" ~w:screen_width ~h:screen_height Sdl.Window.windowed ));

    (* Get surface from Window *)
    let window = deref_option window_p in
    surface_p := Some (manage_result (Sdl.get_window_surface window));
    ()

let load_media surface_p path=
    surface_p := Some (manage_result (Sdl.load_bmp path))

let close windows_p surfaces_p =
    List.iter ( fun x -> Sdl.destroy_window (deref_option x); x:= None) windows_p;
    List.iter ( fun y -> Sdl.free_surface (deref_option y ); y:= None) surfaces_p;
    ()

(* Main  *)
let () =
    (* Pointers *)
    let window_p = ref None in
    let surface_p = ref None in
    (* Background image surface *)
    let bg_p = ref None in

    (* Lists *)
    (* Windows list *)
    let windows_p = [window_p] in
    (* Surfaces list not associated with window *)
    let surfaces_p = [bg_p] in

    (* Events *)
    let ev = Some (Sdl.Event.create ()) in

    initialization window_p surface_p;

    load_media bg_p bg_path;

    let rec game b =
        let over = ref false in
        if not b then (
            if not (Sdl.poll_event ev) then (
                match ev with
                | Some e ->
                    if (Sdl.Event.get e Sdl.Event.typ) = Sdl.Event.quit then
                        over := true
                | None ->
                    ()
            );
            manage_result (Sdl.blit_surface (deref_option bg_p) None (deref_option surface_p) None);
            manage_result (Sdl.update_window_surface (deref_option window_p));
            game !over
        )
        else ()
    in

    game false;

    close windows_p surfaces_p