let small_l j x l =
  let rec aux j x k (h::t) n = if k = n + 1 then 1. else 
    if j = k then aux j x (k + 1) t n else let q = (x -. List.nth l k) /. (List.nth l j -. List.nth l k) in 
    q *. aux j x (k + 1) t n in
  aux j x 0 l (List.length l)

let big_l xl yl = 
  let aux j n xl yl = if j = n + 1 then 0. else List.nth yl j *. small_l j (List.nth xl j) xl in 
  aux 0 (List.length xl) xl yl

let rec only_keys l = match l with 
 [] -> [] |
 (a, b)::xs -> a::only_keys xs
 
let rec only_values l = match l with 
 [] -> [] |
 (a, b)::xs -> b::only_keys xs

let lagrange l = big_l (only_keys l) (only_values l)