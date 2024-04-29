(* Tail recursion and shit *)
let rec fac n = 
  let rec aux m acc = match m with 
    1 -> acc |
    x -> aux (x - 1) (x * acc) in 
  aux n 1;;

let rec remove a lst = 
  let rec aux a l acc = match l with 
    [] -> acc | 
    x::xs -> if x = a then aux a xs acc else aux a xs (x::acc) in 
  aux a lst [];;

let rec partition f l = match l with 
  [] -> ([], []) | 
  x::xs -> let a,b = partition f xs in if f x then x::a,b else a,x::b;;

let rec partition func lst =
  let rec aux f l acc1 acc2 = match l with 
    [] -> (acc1, acc2) |
    x::xs -> if f x then aux f xs (x::acc1) acc2 else aux f xs acc1 (x::acc2) in 
  aux func lst [] [];;


(* Folds *)
(* Do stuff  *)

