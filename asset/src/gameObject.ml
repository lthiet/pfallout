type gameObjectAttribute = {
    x : int;
    y : int;
    z : int
}

type gameObject = gameObjectAttribute

let create_game_object x y z =
{
    x = x;
    y = y;
    z = z;
}

let get_x go = go.x
let get_y go = go.y
let get_z go = go.z

let set_x go x = { go with x = x}
let set_y go y = { go with y = y}
let set_z go z = { go with z = z}