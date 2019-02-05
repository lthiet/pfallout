open Utils

(* Heavily inspired by https://www.redblobgames.com/grids/hexagons/#coordinates as of 31/01/2019 *)
module MHex =
struct
    let size = 80
    let width = round ((sqrt 3.) *. float (size))
    let height = 2 * size

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

    let axial_to_screen_coord (axial : axial_coord) =
        let x = truncate ((float size) *. ((sqrt 3.) *. (float axial.q) +. ((sqrt 3.) /. 2.) *. (float axial.r))) in
        let y = truncate ((float size) *. ((3. /. 2.) *. ( float axial.r) ) ) in
        x,y

   
    let right = {
            x = 1;
            y = -1;
            z = 0;
        }

    let top_right = {
            x = 1;
            y = 0;
            z = -1;
        }

    let top_left = {
            x = 0;
            y = 1;
            z = -1;
        }

    let left = {
            x = -1;
            y = 1;
            z = 0;
        }

    let bot_left = {
            x = -1;
            y = 0;
            z = 1;
        }

    let bot_right = {
            x = 0;
            y = -1;
            z = 1;
        }

    let directions = [
        left;top_right;top_left;right;bot_right;bot_left
    ]

    let rec range_cu n cu =
        if n < 0 then
            []
        else
        let rec aux1 x acc1 = 
            if x > n then
                acc1
            else
                let bound_bot = max (-n) (-x-n) in
                let bound_top = min n (-x+n) in
                let rec aux2 y acc2 = 
                    if y > bound_top then
                        acc2
                    else
                        let z = -x-y in
                        if x = 0 && y = 0 && z = 0 then
                            aux2 (y+1) acc2
                        else
                            aux2 (y+1) ({
                                x = cu.x + x;
                                y = cu.y + y;
                                z = cu.z + z
                            } :: acc2)
                in
                let tmp = aux2 bound_bot [] in
                aux1 (x+1) (tmp @ acc1)
        in
        aux1 (-n) []



    let range_ax n ax =
        let cu = axial_to_cube ax in
        range_cu n cu



end
;;