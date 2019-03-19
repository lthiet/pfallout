
module MInterface = struct

    type t = 
        |Fenetre of Mwdw.t
        |Btn of Mbtn.t;

    let render renderer interface texture =
        match interface with
        | Fenetre -> Mwdw.render renderer interface texture
        | Btn -> MBtn.render renderer interface texture