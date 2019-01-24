open Utils
open Tsdl
open Texture_wrapper

(* A module about a dot that can move *)
module LDot =
struct
    
    (* The type that represents the dots *)
    type t = {
        w : int;
        h : int;
        max_vel : int;
        mutable x : int;
        mutable y : int;
        mutable vel_x : int;
        mutable vel_y : int
    }
    let default_w = 20
    let default_h = 20
    let default_max_vel = 1

    let default = {
        w = default_w;
        h = default_h;
        max_vel = default_max_vel;
        x = 400;
        y = 400;
        vel_x = 0;
        vel_y = 0
    }

    (* Create a dot *)
    let create ?(w:int = default_w) ?(h:int = default_h) ?(max_vel:int = default_max_vel) () =
        {
            default with
            w = w;
            h = h;
            max_vel = max_vel;
        }

    (* Handle the events for dots *)
    let handle_event t e =

        (* Velocity variables for shortcuts *)
        let vx = t.vel_x in
        let vy = t.vel_y in
        let v = t.max_vel in

        (* Check if keyboard has been pressed and if no repeat *)
        if check_ev_type e Sdl.Event.key_down && check_ev_key_repeat e then 
            let k = get_scancode e in
            if k = Sdl.Scancode.up then
                    t.vel_y <- vy - v
            else if k = Sdl.Scancode.down then 
                    t.vel_y <- vy + v
            else if k = Sdl.Scancode.right then 
                    t.vel_x <- vx + v
            else if k = Sdl.Scancode.left then 
                    t.vel_x <- vx - v
            else
                ()
        else if check_ev_type e Sdl.Event.key_up && check_ev_key_repeat e then
            Printf.printf "%s" "lol";
            let k = get_scancode e in
            if k = Sdl.Scancode.up then
                    t.vel_y <- vy + v
            else if k = Sdl.Scancode.down then 
                    t.vel_y <- vy - v
            else if k = Sdl.Scancode.right then 
                    t.vel_x <- vx - v
            else if k = Sdl.Scancode.left then 
                    t.vel_x <- vx + v

    let move t sw sh =
        let new_x =
            (* Potential x *)
            let pot_x = t.x + t.vel_x in
            if pot_x < 0 || pot_x + t.w > sw then
                pot_x - t.vel_x
            else
                pot_x
        in

        let new_y =
            (* Potential y *)
            let pot_y = t.y + t.vel_y in
            if pot_y < 0 || pot_y + t.w > sh then
                pot_y - t.vel_y
            else
                pot_y
        in
        
        t.x <- new_x;
        t.y <- new_y


    let render dot renderer texture =
        LTexture.render renderer
        ~x:(dot.x)
        ~y:(dot.y)
        texture
end