type tree = Leaf | Node of int * tree * tree;;
let root = Node (2, Node (1, Leaf, Leaf), Node (4, Leaf, Node (6, Node (5, Leaf, Leaf), Leaf)));;
let root2 = Node (3, Node (7, Leaf, Leaf), Node (8, Leaf, Node (4, Node (9, Leaf, Leaf), Leaf)));;
let list = [6; 3; 5; 1; 2; 3; 7; 8];;

let rec insert x t = match t with 
  Leaf -> Node (x, Leaf, Leaf) |
  Node (e, l, r) -> if x < e then Node(e, insert x l, r) else Node(e, l, insert x r);;

let rec find x t = match t with 
  Leaf -> false |
  Node (e, l, r) -> e = x || find x l || find x r;;

let rec findMax t = 
  let max a b = if a > b then a else b in 
  match t with 
    Leaf -> min_int |
    Node (x, l, r) -> max x (max (findMax l) (findMax r));;

let rec findMin t =
  let min a b = if a < b then a else b in 
  match t with 
    Leaf -> max_int |
    Node (x, l, r) -> min x (min (findMin l) (findMin r));;

let rec height t =
  let max a b = if a > b then a else b in 
  match t with 
    Leaf -> 0 |
    Node (x, l, r) -> 1 + max (height l) (height r);;

let rec toList t = match t with
  Leaf -> [] | 
  Node (x, l, r) -> (toList l) @ [x] @ (toList r);;

let rec fromList l = match l with 
  [] -> Leaf |
  x::xs -> insert x (fromList xs);;

let concat t1 t2 = fromList ((toList t1) @ (toList t2));; (* Used functions: Line 32, Line 36 *)

let rec delete x t = (* Used function: Line 40 *)
  let rec saveTree x t = match t with 
    Leaf -> Leaf |
    Node (y, l , r) -> if x < y then saveTree x l else if x > y then saveTree x r else 
      match r with Node (z, rl, rr) -> Node (z, concat l rl, rr) | Leaf -> 
        match l with Node (k, ll, lr) -> Node (k, ll, concat lr r) | Leaf -> Leaf in 
  let tmp = saveTree x t in 
  match t with 
    Leaf -> Leaf |
    Node (y, l, r) -> if x = y then tmp else if x < y then Node(y, delete x l, r) else Node(y, l, delete x r);;

















(* Infinite Tree *)
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