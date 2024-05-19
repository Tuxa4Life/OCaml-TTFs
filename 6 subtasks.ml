let f1 acc (a, b) = acc @ [(b, a)];;
List.fold_left (fun acc (a, b) -> acc @ [(b, a)]) [] [("One", 1); ("Two", 2); ("Three", 3)];;

let f2 acc x = List.rev acc @ [x];;
List.fold_left (fun acc x -> List.rev (acc @ [x])) [] [0; 1; 2; 3; 4; 5; 6];;

let f3 = fun acc (a, b) -> (fun a -> b)
List.fold_left (fun acc (a, b) -> (fun a -> b)) (fun _ -> 0) [(1, 5); (2, 4); (3, 3)];;

let map f l =
  let rec map_tl f l acc = match l with 
    [] -> acc | 
    x::xs -> map_tl f xs (acc @ [f x]) in 
  map_tl f l [];;

let replicate n x = 
  let rec replicate_tl n x acc = match n with 
    0 -> acc | 
    _ -> replicate_tl (n - 1) x (x::acc) in 
  replicate_tl n x [];;




type 'a custom_llist = (unit -> 'a custom_cell)
and 'a custom_cell = NilC | ConsC of ('a * 'a custom_llist)

let tester = ConsC (0, fun () -> ConsC (1, fun () -> ConsC (1, fun () -> ConsC (2, fun () -> ConsC (3, fun () -> ConsC (3, fun () -> NilC))))))

let rec custom_map f ll = match ll with
  NilC -> NilC|
  ConsC (x, g) -> ConsC (f x, fun () -> custom_map f (g()))

let rec custom_merge ll1 ll2 = match ll1, ll2 with 
  NilC, NilC -> NilC | 
  ConsC (x, f), NilC -> ConsC (x, fun () -> custom_merge (f()) ll2) | 
  NilC, ConsC (x, f) -> ConsC (x, fun () -> custom_merge ll1 (f())) | 
  ConsC (x, f), ConsC (y, g) -> if x < y then ConsC (x, fun () -> custom_merge (f()) ll2) else ConsC (y, fun () -> custom_merge ll1 (g()))

let rec custom_drop ll = match ll with 
  NilC -> NilC |
  ConsC (x, f) -> match (f()) with 
    NilC -> ConsC (x, fun () -> NilC) | 
    ConsC (y, _) -> if x = y then custom_drop (f()) else ConsC (x, fun () -> custom_drop (f()))




let is_hamming_number n =
  let rec get_factors n i acc = match n with 
    1 -> acc |
    x -> if x mod i = 0 then get_factors (x / i) i (i::acc) else get_factors x (i + 1) acc in 
  let rec has_hamming_factors l = match l with 
    [] -> true | 
    x::xs -> if x = 2 || x = 3 || x = 5 then has_hamming_factors xs else false in
  has_hamming_factors (get_factors n 2 [])

let rec hamming ll = match ll with 
  NilC -> NilC | 
  ConsC (x, f) -> if is_hamming_number x then ConsC (x, fun () ->  hamming (f())) else hamming (f())