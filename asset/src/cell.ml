type cell = {
    x: int;
    y: int
}

let create_cell x y =
{
    x = x;
    y = y
}

let get_x c = c.x
let get_y c = c.y
