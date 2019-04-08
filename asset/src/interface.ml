open Button
open Window
open Tree

(* Implements the interface of the game *)

module MInterface = struct

  (* A single element from the interface *)
  type t = {
      (* All the coordinates are relative to the parent *)
      x : int; 
      y : int;
      w : int;
      h : int;
  }

  (* The whole interface currently displayed,
     only the interface at the top of the list can
     be interacted with, the rest is only displayed *)
  type structure = t MTree.tree list
end


