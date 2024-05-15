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
