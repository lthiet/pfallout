open GameObject

type itemAttribute = {
    value : int;
    weight : int
}

type item = ( gameObject * itemAttribute )

let create_item x y z v w =
    let game_object = GameObject.create_game_object x y z in
    (game_object,{
        value = v;
        weight = w
    })

let get_value item =
    match item with
    (_,attr) -> attr.value

let get_weight item =
    match item with
    (_,attr) -> attr.weight

let get_x item =
    let go,attr = item in
    GameObject.get_x go
