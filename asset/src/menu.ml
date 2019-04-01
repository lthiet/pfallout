open Utils
open Tsdl
(* open Tsdl_ttf *)
open Texture_wrapper
open Keyboard_wrapper
open Mouse_wrapper
open Btn
open Interface
open Window

module MMenu = struct


  (*MKeyboard.key_is_pressed e Sdl.Scancode.escape*)

  let ev = Some (Sdl.Event.create ())

  type context = {
    over : bool;
    map_size : int;
    btn_start : MBtn.t;
    btn_settings : MBtn.t;
    btn_return : MBtn.t;
    btn_Smapsize : MBtn.t;
    btn_Mmapsize : MBtn.t;
    btn_Lmapsize : MBtn.t;
    window_menu : MWindow.t;
    window_settings: MWindow.t;
    window : Sdl.window;
    interface : MInterface.tree;
    current_interface : MInterface.tree
  }

  (*Checks is the mouse is above a button*)
  let check_mouse_above_button button event =
    let x,y,w,h = MBtn.get_coord button in
    if MMouse.is_inside event x y w h then (
      if check_ev_type event Sdl.Event.mouse_button_down then
        {
          button with
          status = MBtn.PRESSED
        }
      else if check_ev_type event Sdl.Event.mouse_button_up then
        {
          button with
          status = MBtn.RELEASED
        }
      else
        button
    )
    else
      {
        button with
        status = MBtn.IDLE
      }

  let update_context context = 
    if Sdl.poll_event ev then (
      Printf.printf("evstart");
      print_newline();
      match ev with
      (* If no event, nothing to do *)
      | None ->
        context
      (* Otherwise, check the event *)
      | Some e -> begin
        (*if the user typed escape*)
        if MKeyboard.key_is_pressed e Sdl.Scancode.escape then (
            (*****************************)
            Printf.printf("ev");
            print_newline();
            let w = (MWindow.create 200 200 500 500 MWindow.SETTINGS) in
            {context with
              interface = MInterface.NODE(
                            (MInterface.WINDOW w),[MInterface.EMPTY])
            }	
        )
        else (
          match context.current_interface with 
          | MInterface.EMPTY -> context;
          | MInterface.NODE(t,l) -> begin
            match t with
            |MInterface.WINDOW w -> begin 
              Printf.printf("window");
              print_newline();
              match MWindow.get_role w with 
                (* If we're currently in the starting menu*)
              | START -> begin
                Printf.printf("start");
                print_newline();
                (* Check if user mouse is above start button *)
                let btn_start = check_mouse_above_button context.btn_start e
                in
                (* Check if user mouse if above settings button *)
                let btn_settings = check_mouse_above_button context.btn_settings e
                in
                (*
                (******************************)
                (* Check if the user wants to go to the Settings menu *)
                let menu_type = if MBtn.is_released btn_settings then Settings
                                else context.menu_type
                in
                *)

                if MBtn.is_released btn_settings then begin (Printf.printf("ok");
                                                        print_newline()) end;
                let interface = if MBtn.is_released btn_settings 
                                  then (MInterface.NODE ( MInterface.ROOT, [
                                              MInterface.NODE (MInterface.WINDOW(context.window_settings), [
                                              MInterface.NODE (MInterface.BTN(context.btn_Smapsize), [MInterface.EMPTY]);
                                              MInterface.NODE (MInterface.BTN(context.btn_Mmapsize), [MInterface.EMPTY]);
                                              MInterface.NODE (MInterface.BTN(context.btn_Lmapsize), [MInterface.EMPTY])
                                                ])
                                              ]) )
                                else context.interface
                                          
                in

                let current_interface = if MBtn.is_released btn_settings 
                                          then (MInterface.NODE (MInterface.WINDOW(context.window_settings), [
                                                    MInterface.NODE (MInterface.BTN(context.btn_Smapsize), [MInterface.EMPTY]);
                                                    MInterface.NODE (MInterface.BTN(context.btn_Mmapsize), [MInterface.EMPTY]);
                                                    MInterface.NODE (MInterface.BTN(context.btn_Lmapsize), [MInterface.EMPTY])
                                                ]) )
                                        else context.current_interface
                in

                (* If the user clicks the red cross button, the game closes *)
                let over = check_ev_type e Sdl.Event.quit || MBtn.is_released btn_start 
                in
                {context with
                  over = over;
                  btn_start = btn_start;
                  btn_settings = btn_settings;
                  interface = interface;
                  current_interface = current_interface;
                }			
                end
              (* If we're currently in the settings menu*)
              | SETTINGS -> begin
                (* Check if user mouse is above return button *)
                let btn_return = check_mouse_above_button context.btn_return e
                in

                (* Check if user mouse if above small map size button *)
                let btn_Smapsize = check_mouse_above_button context.btn_Smapsize e
                in

                (* Check if user mouse if above medium map size button *)
                let btn_Mmapsize = check_mouse_above_button context.btn_Mmapsize e
                in

                (* Check if user mouse if above large map size button *)
                let btn_Lmapsize = check_mouse_above_button context.btn_Lmapsize e
                in

                let map_size = if MBtn.is_released btn_Smapsize then 4
                  else if MBtn.is_released btn_Mmapsize then 6
                  else if MBtn.is_released btn_Lmapsize then 8
                  else context.map_size
                in		

                let interface = if MBtn.is_released btn_return 
                                  then (MInterface.NODE ( MInterface.ROOT, [
                                    MInterface.NODE (MInterface.WINDOW(context.window_menu), [
                                    MInterface.NODE (MInterface.BTN(context.btn_start), [MInterface.EMPTY]);
                                    MInterface.NODE (MInterface.BTN(context.btn_settings), [MInterface.EMPTY])
                                      ])
                                    ]) )
                                else context.interface
                in

                let current_interface = if MBtn.is_released btn_return 
                                          then (MInterface.NODE (MInterface.WINDOW(context.window_menu), [
                                            MInterface.NODE (MInterface.BTN(context.btn_start), [MInterface.EMPTY]);
                                            MInterface.NODE (MInterface.BTN(context.btn_settings), [MInterface.EMPTY])
                                            ]) )
                                        else context.current_interface
                in
                
                let over = check_ev_type e Sdl.Event.quit in

                {context with
                  over=over;
                  map_size = map_size;
                  btn_return = btn_return;
                  btn_Smapsize = btn_Smapsize;
                  btn_Mmapsize = btn_Mmapsize;
                  btn_Lmapsize = btn_Lmapsize;
                  interface = interface;
                  current_interface = current_interface;
                }		
                end
            end
            (* If the current interface is a button or root, we don't do anything*)
            |MInterface.BTN b -> context;
            |MInterface.ROOT -> context;
          end
        )
      end
    )				
    else
      context


  type result = {
    start_game : bool;
    map_size : int;
    window : Sdl.window
  }

  let get_window result = result.window
	let get_map_size result = result.map_size

  let compute_result ctx =
    {
      start_game = MBtn.is_released ctx.btn_start;
      map_size = ctx.map_size;
      window = ctx.window;
    }

  let rec loop renderer context = 
    let new_ctx = update_context context in
    if not new_ctx.over then (
      (* Update the context *)
      (* Clear *)
      manage_result (Sdl.set_render_draw_color renderer 255 255 255 255) "Error : %s";
      manage_result (Sdl.render_clear renderer) "Error : %s";
      MInterface.render renderer new_ctx.interface;

      (* Update the renderer *)
      Sdl.render_present renderer;
      (* Continue the game *)
      loop renderer new_ctx 

      (**
      match new_ctx.menu_type with
      | Settings -> 
        (* Display the settings background *)
        MTexture.render renderer textures.settings_bg;
        (* Display the return button *)
        MBtn.render renderer new_ctx.btn_return textures.btn_return;
        (* Display the small mapsize button *)
        MBtn.render renderer new_ctx.btn_Smapsize textures.btn_Smapsize;
        (* Display the medium mapsize button *)
        MBtn.render renderer new_ctx.btn_Mmapsize textures.btn_Mmapsize;
        (* Display the large mapsize button *)
        MBtn.render renderer new_ctx.btn_Lmapsize textures.btn_Lmapsize;

        (* Update the renderer *)
        Sdl.render_present renderer;
        (* Continue the game *)
        loop renderer new_ctx textures
      (* Load the start menu *)
      | Menu ->
        (* Display the menu background *)
        MTexture.render renderer textures.bg;
        (* Display the start button *)
        MBtn.render renderer new_ctx.btn_start textures.btn;
        (* Display the start button text *)
        (* MBtn.render_text renderer new_ctx.btn_start textures.btn_start_text; *)

        MBtn.render renderer new_ctx.btn_settings textures.btn_settings;

        (* Update the renderer *)
        Sdl.render_present renderer;
        (* Continue the game *)
        loop renderer new_ctx textures
        **)
        
    )
    else
      compute_result new_ctx
      

  (* let font_path = "asset/font/spiderman.ttf" *)

  let run renderer window = 
    (* let font = manage_result (Ttf.open_font font_path 70) "Error font %s" in *)
    (* Create the menu *)

    let window_menu = MWindow.create 0 0 1920 1080 MWindow.START in
    let window_settings = MWindow.create 0 0 1920 1080 MWindow.SETTINGS in
    let btn_start = MBtn.create (960+ (MBtn.width/2)) 750 MBtn.START in
    let btn_settings = MBtn.create (960- 3*(MBtn.width/2)) 750 MBtn.SETTINGS in
    let btn_return = MBtn.create (960- (MBtn.width/2)) 750 MBtn.OPTION in (**********MBTN OPTION INUTILE ?*********)
    let btn_Smapsize = MBtn.create (480 - (MBtn.width/2)) 200 MBtn.OPTION in
    let btn_Mmapsize = MBtn.create (960- (MBtn.width/2)) 80 MBtn.OPTION in
    let btn_Lmapsize = MBtn.create (1440- (MBtn.width/2)) 200 MBtn.OPTION in

    let ctx  = {
      over = false;
      map_size = 6;
      btn_start = btn_start;
      btn_settings = btn_settings;
      btn_return = btn_return;
      btn_Smapsize = btn_Smapsize;
      btn_Mmapsize = btn_Mmapsize;
      btn_Lmapsize = btn_Lmapsize;
      window_menu = window_menu;
      window_settings = window_settings;
      interface = MInterface.NODE ( MInterface.ROOT, [
                                    MInterface.NODE (MInterface.WINDOW(window_menu), [
                                    MInterface.NODE (MInterface.BTN(btn_start), [MInterface.EMPTY]);
                                    MInterface.NODE (MInterface.BTN(btn_settings), [MInterface.EMPTY])
                                      ])
                                    ]);
      current_interface = MInterface.NODE (MInterface.WINDOW(window_menu), [
                                    MInterface.NODE (MInterface.BTN(btn_start), [MInterface.EMPTY]);
                                    MInterface.NODE (MInterface.BTN(btn_settings), [MInterface.EMPTY])
                                      ]);
      window = window
    } in
    loop renderer ctx;
end;;