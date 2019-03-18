open Utils
open Tsdl
(* open Tsdl_ttf *)
open Texture_wrapper
open Keyboard_wrapper
open Mouse_wrapper
open Btn

module MMenu = struct
  let ev = Some (Sdl.Event.create ())

  type context = {
    over : bool;
    btn_start : MBtn.t;
	
	(******************)
	settings : bool;
	btn_settings : MBtn.t;
	map_size : int;
	
	btn_return : MBtn.t;
	btn_Smapsize : MBtn.t;
	btn_Mmapsize : MBtn.t;
	btn_Lmapsize : MBtn.t;
  }

  type textures = {
    bg : MTexture.t;
    btn : MTexture.t;
	(************)
	btn_settings : MTexture.t;
	
	settings_bg : MTexture.t;
    btn_return : MTexture.t;
	btn_Smapsize : MTexture.t;
	btn_Mmapsize : MTexture.t;
	btn_Lmapsize : MTexture.t;
	
    (* btn_start_text : MTexture.t *)
  }

  (*************)
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
    if Sdl.poll_event ev then
      match ev with
      (* If no event, nothing to do *)
      | None ->
        context
      (* Otherwise, check the event *)
      | Some e ->
		(******)
		if not context.settings then (
			(* Check if user mouse is above start button *)
			let btn_start = check_mouse_above_button context.btn_start e
			(*let x,y,w,h = MBtn.get_coord context.btn_start in
			let btn_start =
			  if MMouse.is_inside e x y w h then 
				if check_ev_type e Sdl.Event.mouse_button_down then
				  {
					context.btn_start with
					status = MBtn.PRESSED
				  }
				else if check_ev_type e Sdl.Event.mouse_button_up then
				  {
					context.btn_start with
					status = MBtn.RELEASED
				  }
				else
				  context.btn_start
			  else
				{
				  context.btn_start with
				  status = MBtn.IDLE
				}
				*)
			in
			
			
			(************)
			(* Check if user mouse if above settings button *)
			let btn_settings = check_mouse_above_button context.btn_settings e
			in
			
			let settings = MBtn.is_released btn_settings
			in
			
			(* If the user clicks the red cross button, the game closes *)
			let over = check_ev_type e Sdl.Event.quit || MBtn.is_released btn_start in
			{
			  over = over;
			  btn_start = btn_start;
			  (*********)
			  settings = settings;
			  btn_settings = btn_settings;
			  map_size = context.map_size;
			  btn_return = context.btn_return;
			  btn_Smapsize = context.btn_Smapsize;
			  btn_Mmapsize = context.btn_Mmapsize;
			  btn_Lmapsize = context.btn_Lmapsize
			}			
		)
		else (
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
			
			let settings = not (MBtn.is_released btn_return)
			in
			
			let map_size = if MBtn.is_released btn_Smapsize then 4
						else if MBtn.is_released btn_Mmapsize then 6
						else if MBtn.is_released btn_Lmapsize then 8
						else context.map_size
			in		
			
			{
			  over = context.over;
			  btn_start = context.btn_start;
			  settings = settings;
			  btn_settings = context.btn_settings;
			  map_size = map_size;
			  btn_return = btn_return;
			  btn_Smapsize = btn_Smapsize;
			  btn_Mmapsize = btn_Mmapsize;
			  btn_Lmapsize = btn_Lmapsize
			}						
		)
		
    else
      context
	  
	
  type result = {
    start_game : bool;
	
	(***********)
	map_size : int;
  }

  let compute_result ctx =
    {
      start_game = MBtn.is_released ctx.btn_start;
	  
	  (*****************)
	  map_size = ctx.map_size;
    }

  let rec loop renderer context textures = 
    let new_ctx = update_context context in
    if not new_ctx.over then (
      (* Update the context *)
      (* Clear *)
      manage_result (Sdl.set_render_draw_color renderer 255 255 255 255) "Error : %s";
      manage_result (Sdl.render_clear renderer) "Error : %s";
	  
	  (***********)
	  (* Load the settings menu *)
	  if new_ctx.settings then (
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
	  )
	  
	  (* Load the start menu *)
	  else (
		(* Display the menu background *)
		MTexture.render renderer textures.bg;
		(* Display the start button *)
		MBtn.render renderer new_ctx.btn_start textures.btn;
		(* Display the start button text *)
		(* MBtn.render_text renderer new_ctx.btn_start textures.btn_start_text; *)

		(****************************)
		MBtn.render renderer new_ctx.btn_settings textures.btn_settings;

		(* Update the renderer *)
		Sdl.render_present renderer;
		(* Continue the game *)
		loop renderer new_ctx textures
	  )
    )
    else
      compute_result new_ctx

  let menu_bg_path = "asset/image/menu_bg.png"
  let btn_path = "asset/image/btns.png"
  (**********************)
  let btn_settings_path = "asset/image/btn_settings.png"
  let settings_bg_path = "asset/image/settings_bg.png"
  let btn_return_path = "asset/image/btn_return.png"
  let btn_Smapsize_path = "asset/image/btn_smap.png"
  let btn_Mmapsize_path = "asset/image/btn_mmap.png"
  let btn_Lmapsize_path = "asset/image/btn_lmap.png" 
  
  
  (* let font_path = "asset/font/spiderman.ttf" *)

  let run renderer = 
    (* let font = manage_result (Ttf.open_font font_path 70) "Error font %s" in *)
    (* Create the menu *)
    let txt = {
      bg = MTexture.load_from_file renderer menu_bg_path;
      btn = MTexture.load_from_file renderer btn_path;
	  (* btn_start_text = MTexture.load_from_rendered_text renderer font "Start" (Sdl.Color.create 255 255 255 255) *)
	  (**********************)
	  btn_settings = MTexture.load_from_file renderer btn_settings_path;
      settings_bg = MTexture.load_from_file renderer settings_bg_path;
      btn_return = MTexture.load_from_file renderer btn_return_path;
	  btn_Smapsize = MTexture.load_from_file renderer btn_Smapsize_path;
	  btn_Mmapsize = MTexture.load_from_file renderer btn_Mmapsize_path;
	  btn_Lmapsize = MTexture.load_from_file renderer btn_Lmapsize_path;
    } in
    let ctx  = {
      over = false;
      btn_start = MBtn.create (960+ (MBtn.width/2)) 750 MBtn.START;
	  (*****************)
	  settings = false;
	  btn_settings = MBtn.create (960- 3*(MBtn.width/2)) 750 MBtn.OPTION;
	  btn_return = MBtn.create (960- (MBtn.width/2)) 750 MBtn.OPTION; (**********MBTN OPTION INUTILE ?*********)
	  btn_Smapsize = MBtn.create (480 - (MBtn.width/2)) 200 MBtn.OPTION;
	  btn_Mmapsize = MBtn.create (960- (MBtn.width/2)) 80 MBtn.OPTION;
	  btn_Lmapsize = MBtn.create (1440- (MBtn.width/2)) 200 MBtn.OPTION;
	  map_size = 6;
    } in
    loop renderer ctx txt;
end;;