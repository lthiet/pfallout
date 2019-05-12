open Tsdl
open Tsdl_ttf
open Utils


module MFont_pack = struct
  let good_times = "asset/font/goodtimes.ttf"

  let open_font path size = 
    manage_result (Ttf.open_font path size) "Error font %s"

  let open_font_with_outline path size outline_size= 
    let f1 = open_font path size in
    let f2 = open_font path size in
    let () = Ttf.set_font_outline f2 outline_size in
    f1,f2

end