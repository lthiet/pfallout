open Tsdl
open Utils
open Texture_wrapper

module LButton = struct
    type lButtonSprite =
    | BUTTON_SPRITE_MOUSE_OUT
    | BUTTON_SPRITE_MOUSE_OVER_MOTION
    | BUTTON_SPRITE_MOUSE_DOWN
    | BUTTON_SPRITE_MOUSE_UP
    | BUTTON_SPRITE_MOUSE_TOTAL

    let btn_sprite_to_int e =
        match e with
        | BUTTON_SPRITE_MOUSE_OUT -> 0 
        | BUTTON_SPRITE_MOUSE_OVER_MOTION -> 1
        | BUTTON_SPRITE_MOUSE_DOWN -> 2
        | BUTTON_SPRITE_MOUSE_UP -> 3
        | BUTTON_SPRITE_MOUSE_TOTAL -> 4

    type t = {
        mPosition : Sdl.point;
        mCurrentSprite : lButtonSprite
    }

    let default = {
        mPosition = Sdl.Point.create ~x:0 ~y:0 ;
        mCurrentSprite = BUTTON_SPRITE_MOUSE_OUT
    }

    let button_width = 300
    let button_height = 200
    let total_button = 4

    let set_pos t x y =
    {
        t with mPosition =
        Sdl.Point.create ~x:x ~y:y
    }

    let get_pos t =
        (
            Sdl.Point.x t.mPosition,
            Sdl.Point.y t.mPosition
        )

    let handle_event t ev =
        let ev_type = Sdl.Event.get ev Sdl.Event.typ in 
        let b = 
            ev_type = Sdl.Event.mouse_motion 
            || ev_type = Sdl.Event.mouse_button_down 
            || ev_type = Sdl.Event.mouse_button_up 
        in

        if b then
            let state,(x,y) = Sdl.get_mouse_state () in
            let mouse_x,mouse_y = get_pos t in
            let inside = 
                (* Mouse is left of button *)
                if x < mouse_x then
                    false
                (* Mouse is right of button *)
                else if x > mouse_x + button_width then
                    false
                else if y < mouse_y then
                    false
                else if y > mouse_y + button_height then
                    false
                else 
                    true
            in
            
            let new_current_sprite =
            if not inside then
                BUTTON_SPRITE_MOUSE_OUT
            else
                match ev_type with
                | x when x = Sdl.Event.mouse_motion -> BUTTON_SPRITE_MOUSE_OVER_MOTION
                | x when x = Sdl.Event.mouse_button_down -> BUTTON_SPRITE_MOUSE_DOWN
                | x when x = Sdl.Event.mouse_button_up -> BUTTON_SPRITE_MOUSE_UP
                | _ -> BUTTON_SPRITE_MOUSE_OUT
            in
            {t with mCurrentSprite = new_current_sprite}
        else
            t

    let render renderer t texture clips =
        let clip =
            let i = btn_sprite_to_int t.mCurrentSprite in
            clips.(i)
        in
        let p = t.mPosition in
        LTexture.render renderer
            ~clip:(Some (clip))
            ~x:(Sdl.Point.x p)
            ~y:(Sdl.Point.y p)
        texture
end