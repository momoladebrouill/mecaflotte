type 'a t = {
    tab:'a array array;
    default:'a;
    width:int;
    height:int;
}

let make n m init = {
    tab = Array.init n (fun _ -> Array.make m init);
    width = n;
    height = m;
}

let iteri f grid =
    Array.iteri (fun i row ->
        Array.iteri (fun j cell ->
            f (i,j) cell)
        row)
    grid.tab

let mapi f grid = {
    tab = Array.mapi (fun i row ->
        Array.mapi (fun j cell ->
            f (i,j) cell)
        row)
    grid.tab;
    width = grid.width;
    height = grid.height;
}

let fold_left f v0 grid =
    Array.fold_left (fun acc1 t ->
        Array.fold_left (fun acc2 e ->
            f acc2 e) acc1 t)
    v0 grid.tab

let set grid x y v =
    grid.tab.(x).(y) <- v

let get grid x y =
    if x < 0 || x >= grid.width || y < 0 || y >= grid.height then
        grid.default
    else
        grid.tab.(x).(y)
