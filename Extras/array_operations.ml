let rec length l = match l with 
  [] -> 0 |
  _::xs -> 1 + length xs;;

let rec at i l = match l with 
  [] -> failwith "Index out of Bounds" |
  x::xs -> if i = 0 then x else at (i - 1) xs;;

let rec concat l1 l2 = match l1 with 
  [] -> l2 |
  x::xs -> x::concat xs l2;;

let rec every f l = match l with 
  [] -> true |
  x::xs -> if f x then every f xs else false;;

let rec fill_all n l = match l with 
  [] -> [] |
  _::xs -> n::fill_all n xs;;

let rec fill start finish n l = if start > finish then failwith "Wrong arguments" else
  match l with 
  [] -> [] |
  x::xs -> if start <= 0 && finish >= 0 then n::fill (start - 1) (finish - 1) n xs else x::fill (start - 1) (finish - 1) n xs;;

let rec filter f l = match l with 
  [] -> [] |
  x::xs ->  if f x then x::filter f xs else filter f xs;;

let rec find f l = match l with 
  [] -> failwith "Element not found" | 
  x::xs -> if f x then x else find f xs;;

let find_index f l =
  let rec aux f l i = match l with 
    [] -> failwith "Element not found" |
    x::xs -> if f x then i else aux f l (i + 1) in
  aux f l 0;;

let rec find_last f l = 
  let rec rev lst = match lst with 
    [] -> [] |
    x1::xs1 -> rev xs1 @ [x1] in 
  match rev l with 
    [] -> failwith "Element not found" | 
    x::xs -> if f x then x else find_last f (rev xs);;

let find_last_index f l = 
  let rec rev lst = match lst with 
    [] -> [] |
    x1::xs1 -> rev xs1 @ [x1] in 
  let rec aux f l i = match rev l with 
    [] -> failwith "Element not found" | 
    x::xs -> if f x then i else aux f xs (i - 1) in 
  let rec length l = match l with 
    [] -> 0 |
    _::xs -> 1 + length xs in 
  aux f l ((length l) - 1);;

let rec map f l = match l with 
  [] -> [] |
  x::xs -> f x::map f xs;;

let rec pop l = match l with 
  [] -> [] |
  [x] -> [] |
  x::xs -> x::pop xs;;

let rec push n l = match l with 
  [] -> [n] |
  x::xs -> x::push n xs;;

let reduce l = 
  let rec aux l acc = match l with 
    [] -> acc |
    x::xs -> aux xs (acc + x) in 
  aux l 0;;

let rec slice start finish l = if start > finish then failwith "Wrong arguments" else
  match l with 
    [] -> [] |
    x::xs -> if start <= 0 && finish >= 0 then slice (start - 1) (finish - 1) xs else x::slice (start - 1) (finish - 1) xs;;

let rec some f l = match l with 
  [] -> false |
  x::xs -> if f x then true else some f xs;;

let rec sort l = 
  let rec insert n l = match l with 
    [] -> [n] |
    x::xs -> if n < x then n::l else x::insert n xs in 
  match l with 
    [] -> [] |
    x::xs -> insert x (sort xs);;

let unshift n l = match l with 
  [] -> [n] |
  x::xs -> n::l;;

  