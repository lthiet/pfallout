open Texture_wrapper

module type ELT = sig
    type t
end
;;

module type Binder = sig
    type media_type 
    type e
    type t
end
;;

module MTextureBind ( E : ELT) : Binder with type e = E.t = struct
    type media_type = MTexture.t
    type e = E.t
    type t = ( media_type * e)
    let bind t e =
        t,e
end
;;