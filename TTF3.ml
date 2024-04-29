(* Fun with Folding *)
let rec length l = match l with 
  [] -> 0 |
  _::xs -> 1 + length xs;;

let longest_list ll = 
  let rec length l = match l with 
  [] -> 0 |
  _::xs -> 1 + length xs in
  let rec aux ll longest = match ll with 
    [] -> longest |
    x::xs -> if length x > length longest then aux xs x else aux xs longest in 
  aux ll [];;

let rec swap l = match l with 
  [] -> [] |
  (k, v)::xs -> (v, k)::swap xs;;

let spread l = 
  let rec aux l acc switch = match l with 
    [] -> acc |
    x::xs -> if switch then x::aux xs acc (not switch) else aux xs (acc @ [x]) (not switch) in 
  aux l [] false;;

let rec find_in_list key l = match l with 
  [] -> failwith "Element with input key was not found." |
  (k, v)::xs -> if k = key then v else find_in_list key xs;;

let rec compose fl = 
  let rec rev l = match l with 
    [] -> [] |
    x::xs -> (rev xs)@[x] in
  match rev fl with
  [] -> 0 |
  x::xs -> x (compose xs);;

let compute a l = 
  let rec length l = match l with
    [] -> 0 |
    _::xs -> 1 + length xs in 
  let rec power n p = match p with 
    0 -> 1 |
    _ -> n * power n (p - 1) in 
  let a_coef = power a (power 2 (length l)) in
  let rec comp a l i = match l with 
    [] -> a |
    x::xs -> (power x (power 2 i)) * comp a xs (i + 1) in 
  comp a_coef l 0;;


(* Crawling on trees *)
type binary_tree = Leaf | Node of int * binary_tree * binary_tree;;

let tree = Node (5, Node(2, Node (1, Node (0, Leaf, Leaf), Leaf), Node (3, Leaf, Leaf)), Node (8, Node (7, Node (6, Leaf, Leaf), Leaf), Node(9, Leaf, Leaf)));;

let rec insert n = function
  [] -> n |
  _::xs -> insert n xs;;


let inst = (0, 0, 0, 0);;
let rec crawl tree inst = 5;;

