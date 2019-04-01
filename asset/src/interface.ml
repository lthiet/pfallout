open Window
open Utils
open Tsdl
(* open Tsdl_ttf *)
open Texture_wrapper
open Texture_interface_pack
open Keyboard_wrapper
open Mouse_wrapper
open Btn


module MInterface = struct

    type t = 
        |WINDOW of MWindow.t
        |BTN of MBtn.t
        |ROOT 

    type tree = EMPTY | NODE of t * tree list

    let rec render renderer tree =
        let textures = MTexture_interface_pack.create renderer in
        match tree with
        | NODE(t,l) -> begin
                        match t with 
                        | WINDOW t -> begin
                                    match t.role with
                                    |MWindow.SETTINGS -> 
                                    MTexture.render renderer (MTexture_interface_pack.get_wsettings textures);
                                    |MWindow.START -> 
                                    MTexture.render renderer (MTexture_interface_pack.get_wstart textures);
                                    end
                        | BTN t -> begin
                                    match t.role with 
                                    |MBtn.START -> 
                                    MBtn.render renderer t (MTexture_interface_pack.get_bstart textures);
                                    |MBtn.SETTINGS ->
                                    MBtn.render renderer t (MTexture_interface_pack.get_bsettings textures);
                                    |MBtn.OPTION -> ();
                                    end
                        | ROOT -> ()
                        end;                 
                        List.iter ( fun e -> render renderer e ) l;
        | EMPTY -> ();
end
