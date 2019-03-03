open Tsdl

module MCamera = struct
let vel_max = 20

type t = {
    rect : Sdl.rect;
    vel_x : int;
    vel_y : int
}

let create r =
    {
        rect = r;
        vel_x = 0;
        vel_y = 0
    }

let get_rect t = t.rect
let get_vel_x t = t.vel_x
let get_vel_y t = t.vel_y

end
