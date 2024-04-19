type rect = Sizes of int * int | Square of int;;

let calculate rect = match rect with
  Square x -> x * x |
  Sizes (x, y) -> x * y;;

let rotate rect = match rect with 
  Square x -> rect |
  Sizes (x, y) -> if x >= y then Sizes (y, x) else rect;;

let rec sortByWidth l =
  let getWidth r = match r with Square x -> x | Sizes (x, y) -> x in 
  let rec insert r l = match l with 
    [] -> [r] |
    Square x::xs -> if x > getWidth r then r::l else insert r xs |
    Sizes (x, y)::xs -> if x > getWidth r then r::l else insert r xs in 
  match l with 
    [] -> [] |
    x::xs -> insert x (sortByWidth xs);;

type 'a sequence = Nil | Cons of 'a * ('a sequence);;

let rec seqToList seq = match seq with 
  Nil -> [] |
  Cons (x, xs) -> x::seqToList xs;;

let rec listToSeq l = match l with 
  [] -> Nil |
  x::xs -> Cons(x, listToSeq xs);;

let rec take n seq = if n = 0 then Nil else match seq with
  Nil -> Nil |
  Cons (s, s') -> Cons(s, take (n - 1) s');;

let rec drop n seq = if n = 0 then seq else match seq with 
  Nil -> Nil |
  Cons (s, s') -> drop (n - 1) s';;

let rec map f seq = match seq with 
  Nil -> Nil |
  Cons (s, s') -> Cons (f s, map f s');;





type expr = Num of int | Add of expr * expr | Sub of expr * expr | Mult of expr * expr | Div of expr * expr | Pow of expr * expr;;

let rec eval exp = match exp with 
  Num x -> x |
  Add(e, e') -> eval e + eval e' |
  Sub(e, e') -> eval e - eval e' |
  Mult(e, e') -> eval e * eval e' |
  Div(e, e') -> if eval e' = 0 then failwith "Cannot devide by zero" else eval e / eval e' |
  Pow(e, e') -> eval e;; (* need to use power function *)