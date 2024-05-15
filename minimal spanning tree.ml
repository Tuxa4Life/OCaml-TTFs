type graph = (int * float * int) list

let graph =  [(0, 1., 1); (0, 4. ,2); (1, 2., 2); (1, 1., 3); (2, 3., 3)]

let rec sort_by_weight l = 
  let rec insert (p1, w, p2) l = match l with 
    [] -> [(p1, w, p2)] |
    (x1, w1, x2)::xs -> if w < w1 then (p1, w, p2)::l else (x1, w1, x2)::insert (p1, w, p2) xs in 
  match l with 
    [] -> [] |
    x::xs -> insert x (sort_by_weight xs)

let rec remove t l = match l with 
  [] -> [] | 
  x::xs -> if x = t then xs else x::remove t xs

let rec filter (a, b, c) l = match l with 
  [] -> [] |
  (x, y, z)::xs -> if c = z then (x, y, z)::filter (a, b, c) xs else filter (a, b, c) xs

let head l = match l with 
  [] -> [] |
  x::xs -> [x]

let rec mst l = 
  let rec aux l acc = match l with 
    [] -> acc |
    (a, b, c)::xs -> aux xs (acc @ (head (sort_by_weight (filter (a, b, c) xs)))) in 
  head l @ aux l []