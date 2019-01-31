(* Heavily inspired by https://www.redblobgames.com/grids/hexagons/#coordinates as of 31/01/2019 *)
module MHex =
struct
    type axial_coord = {
        q : int;
        r : int
    }

    type cube_coord = {
        x : int;
        y : int;
        z : int
    }

    let axial_to_cube axial =
        let x = axial.q in
        let y = axial.r in
        let z = -x-y in
        {
            x = x;
            y = y;
            z = z;
        }

    let cube_to_axial cube = 
        let q = cube.x in
        let r = cube.y in
        {
            q = q;
            r = r
        }

   end
;;

module HexGraphics = struct
    let size = 40
    let axial_to_screen_coord (axial : MHex.axial_coord) =
        let x = truncate ((float size) *. ((sqrt 3.) *. (float axial.q) +. ((sqrt 3.) /. 2.) *. (float axial.r))) in
        let y = truncate ((float size) *. ((3. /. 2.) *. ( float axial.r) ) ) in
        x,y
end
;;