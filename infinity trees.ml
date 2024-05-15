type 'a ltree = LNode of 'a * (unit -> 'a ltree) * (unit -> 'a ltree);;
type 'a btree = Leaf | Node of 'a * 'a btree * 'a btree;;

let rec layer_tree r =  LNode (r, (fun () -> layer_tree (r + 1)), (fun () -> layer_tree (r + 1)))

let rec interval_tree l h = LNode((l, h), (fun () -> interval_tree l ((l + h) / 2)), (fun () -> interval_tree ((l + h) / 2) h ))
let rec rational_tree n d = LNode((n, d), (fun () -> rational_tree n (d + 1)), (fun () -> rational_tree (n + 1) d));;
let rec top n (LNode(x, f, g)) = if n = 0 then Leaf else Node (x, (top (n - 1) (f())), (top (n - 1) (g())));;
let rec map f (LNode(x, k, g)) = LNode (f x, (fun () -> map f (k())), (fun () -> map f (g())));;

let rec find x (LNode(v, l, r)) = 
  let rec find_in_list n l = match l with [] -> false | x::xs -> if x = n then true else find_in_list n xs in
  let rec get_layer n (LNode (x, f, g)) = if n = 1 then [x] else (get_layer (n - 1) (f())) @ (get_layer (n - 1) (g())) in 
  let rec aux x (LNode(v, l, r)) depth = if find_in_list x (get_layer depth (LNode(v, l, r))) then true else aux x (LNode(v, l, r)) (depth + 1) in 
  aux x (LNode(v, l, r)) 1;;