module MAnimation :
  sig

    type animation_unit = {
      entity : Entity.MEntity.t;
      fx : Fx.MFx.t option;
      initial_time : int;
      current_time : int;
    }
    val create_animation_unit :
      Entity.MEntity.t -> Fx.MFx.t option -> int -> animation_unit
    type t = { to_be_animated : animation_unit list list; }
    val add : t -> t -> t
    val create : animation_unit list list -> t
    val is_over : t -> bool
    type res = {
      currently_animated : Entity.MEntity.t;
      current_fx : Fx.MFx.t option;
      next_animated : Entity.MEntity.t option;
      time_left : int;
      initial_time_left : int;
    }
    val get_currently_animated : res -> Entity.MEntity.t
    val get_current_fx : res -> Fx.MFx.t option
    val get_current_animated_and_next : t -> res list
    val next_coord_currently_animated : res -> int * int
    val compute_next : t -> t
    val create_nuke_drop :
      Entity.MEntity.t ->
      Hex.MHex.axial_coord -> Entity.MEntity.t list -> int -> int -> int -> t
  end
