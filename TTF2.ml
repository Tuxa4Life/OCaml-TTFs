(* Peano Arithmetic *)

type nat = Zero | Succ of nat;;

let rec toNatural n = match n with
  0 -> Zero |
  _ -> Succ (toNatural (n - 1));;

let rec toInteger x = match x with
  Zero -> 0 |
  Succ (n) -> 1 + toInteger n;;

let rec add n1 n2 = match n1 with 
  Zero -> n2 |
  Succ (x) -> Succ (add x n2);;

let rec mul x y = match x with
  Zero -> Zero |
  Succ (n) -> add (mul n y) y;;

let rec pow x y = match x with
  Zero -> Succ (Zero) |
  Succ (n) -> mul (pow x n) x;;

let rec leq x y = match x, y with
  Succ (n), Succ (m) -> leq n m |
  _ -> x = Zero;;


  (* Operations on Lists *)
let custom_h l = match l with [] -> None | x::_ -> Some x;;

let custom_t l = match l with [] -> None | [x] -> None | _::t -> Some t;;

let rec append l1 l2 = match l1 with  
  [] -> l2 |
  x::xs -> x::append xs l2;;

let custom_reverse_that_he_wanted l = 
  let rec rev l acc = match l with 
    [] -> acc |
    x::xs -> rev xs (x::acc) in 
    
    rev l [];;

let rec custom_reverse l = match l with
  [] -> [] |
  x::xs -> custom_reverse xs @ [x];;

let rec custom_length l = match l with  
  [] -> 0 |
  _::t -> 1 + custom_length t;;

let rec custom_insert l i e = if i = 0 then e::l else match l with 
  [] -> [e] |
  x::xs -> x::custom_insert xs (i - 1) e;;
  

  (* TODO *) (* Quadtrees *)
