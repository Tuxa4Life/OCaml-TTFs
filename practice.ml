type 'a tree = Leaf | Node of 'a * 'a tree * 'a tree;;
let root = Node (2, Node (1, Leaf, Leaf), Node (4, Leaf, Node (5, Leaf, Leaf)));;
let dictoot = Node ((2, "Two"), Node ((1, "One"), Leaf, Leaf), Node ((4, "Four"), Leaf, Node ((5, "Five"), Leaf, Leaf)));;
let dict = [(2, "Two"); (1, "One"); (4, "Four"); (5, "Five"); (0, "Zero")];;

let rec size tree = match tree with 
  Leaf -> 0 |
  Node (_, l, r) -> 1 + size l + size r;;

let rec total tree = match tree with 
  Leaf -> 0 |
  Node (x, l, r) -> x + total l + total r;;

let rec depth tree = 
  let max x y = if x > y then x else y in 
  match tree with 
    Leaf -> 0 |
    Node (x, l, r) -> 1 + max (depth l) (depth r);;

let rec treeToList tree = match tree with 
  Leaf -> [] |
  Node (x, l, r) -> treeToList l @ [x] @ treeToList r;;

let rec mapTree f tree = match tree with 
  Leaf -> Leaf |
  Node (x, l , r) -> Node (f x, mapTree f l, mapTree f r);;

let rec insert n tree = match tree with 
  Leaf -> Node (n, Leaf, Leaf) |
  Node (x, l, r) -> if n < x then Node (x, insert n l, r) else Node (x, l, insert n r);;

let rec search k tree = match tree with 
  Leaf -> failwith "Cannot find value with key" |
  Node ((k', v), l, r) -> if k' = k then v else if k < k' then search k l else search k r;;

let rec is_in n tree = match tree with 
  Leaf -> false |
  Node (x, l, r) -> if x = n then true else if n < x then is_in n l else is_in n r;;

let rec flip tree = match tree with 
  Leaf -> Leaf |
  Node (x, l, r) -> Node (x, flip r, flip l);;

let rec equal t1 t2 = match t1, t2 with 
  Leaf, Leaf -> true | 
  Leaf, Node (x, y, z) -> false | Node (x, y, z), Leaf -> false |
  Node (x1, l1, r1), Node (x2, l2, r2) -> (x1 = x2) && equal l1 l2 && equal r1 r2;;

let rec generalEqual t1 t2 = mapTree (fun x -> 0) t1 = mapTree (fun x -> 0) t2;;

let rec tree_of_list dict = match dict with 
  [] -> Leaf |
  (k, v)::xs -> insert (k, v) (tree_of_list xs);;

let rec combine t1 t2 = match t1, t2 with 
  Leaf, Leaf -> Leaf |
  Leaf, t2 -> t2 | t1, Leaf -> t1 |
  Node ((k, v), l1, r1), Node ((k', v'), l2, r2) -> if k = k' then Node ((k, v), combine l1 l2, combine r1 r2) else 
    if k < k' then Node ((k, v), combine l1 t2, combine r1 t2) else Node ((k', v'), combine t1 l1, combine t1 r2);;


type 'a flexitree = Leaf | Node of 'a * 'a flexitree list;;

let foot = Node (4, [Node (1, [Leaf]); Node (5, [Node (8, [Leaf]); Node (9, [Leaf])])]);;

let fize free = 
  let rec iterate l = match l with 
    Node (_, lst)::xs -> 1 + iterate lst + iterate xs |
    _ -> 0 in 
  match free with 
    Node (_, lst) -> 1 + iterate lst |
    _ -> 0;;

let fotal free = 
  let rec iterate l = match l with 
    Node (x, lst)::xs -> x + iterate lst + iterate xs |
    _ -> 0 in 
  match free with 
    Node (x, lst) -> x + iterate lst |
    _ -> 0;;

let fap f free = 
  let rec iterate f l = match l with 
    Node (x, lst)::xs -> Node (f x, iterate f lst)::iterate f xs |
    _ -> [] in 
  match free with 
    Node (x, lst) -> Node (f x, iterate f lst) |
    _ -> Leaf;;