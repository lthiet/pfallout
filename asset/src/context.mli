val ev : Tsdl.Sdl.event option
module MGameContext :
sig
  type t = {
    over : bool;
    camera : Camera.MCamera.t;
    grid : Grid.MGrid.t;
    cursor_selector : Cursor.MCursor.cursor;
    faction_list : Faction.MFaction.t list;
    faction_controlled_by_player : Faction.MFaction.t;
    action_src : Hex.MHex.axial_coord option;
    action_dst : Hex.MHex.axial_coord option;
    action_layer : Layer_enum.MLayer_enum.t option;
    action_type : Action_enum.MAction_enum.enum option;
    movement_range_selector : Tile.MTile.t list;
    to_be_added : Entity.MEntity.t list;
    to_be_deleted : Entity.MEntity.t list;
    animation : Animation.MAnimation.t;
    new_turn : bool;
    frame : int;
    scale : float;
    interface : Interface.MInterface.structure;
    post_process_interface : Interface.MInterface.structure;
    current_layer : Layer_enum.MLayer_enum.t;
    window : Tsdl.Sdl.window;
    texture_pack : Lru_cache.MLRUCache.t
  }
  val update_context : t -> t
end
