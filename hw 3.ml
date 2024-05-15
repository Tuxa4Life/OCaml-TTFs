let rec member f t l = match l with 
  [] -> false | 
  x::xs -> f t x = 0 || member f t xs;;        

let count_occurrences l =  
  let rec sort l = 
    let rec insert  (a, b) l = match l with 
      [] -> [(a, b)] | 
      (x, y)::xs -> if b >= y then (a, b)::l else (x, y)::insert (a, b) xs in 
    match l with 
      [] -> [] |
      (x, y)::xs -> insert (x, y) (sort xs) in 
  let rec increase a l = match l with 
    [] -> [(a, 1)] |
    (x, y)::xs -> if x = a then (x, y + 1)::xs else (x, y)::increase a xs in 
  let rec aux l acc = match l with 
    [] -> sort acc |
    x::xs -> aux xs (increase x acc) in 
  aux l [];;

let rec drop_last l = match l with 
    [] -> failwith "Empty list has no last element" |
    [x] -> [] |
    x::xs -> x::drop_last xs;;

let rec drop_last_opt l = 
  let rec aux l acc = match l with 
    [] -> None | 
    [x] -> Some (List.rev acc) | 
    x::xs -> aux xs (x::acc) in 
  aux l [];;

let rec zip_with f l1 l2 = match l1, l2 with 
  l, [] -> [] | [], l -> [] |
  x::xs, y::ys -> (f x y)::zip_with f xs ys;;

let rec unzip l = 
  let rec get_keys l  = match l with 
    [] -> [] |
    (a, _)::xs -> a::get_keys xs in 
  let rec get_values l  = match l with 
    [] -> [] |
    (_, b)::xs -> b::get_values xs in 
  (get_keys l, get_values l);;
