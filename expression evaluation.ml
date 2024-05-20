type rat = int * int (* num, denom *)
type binary_op = Add | Sub | Mul | Div
type unary_op = Neg
type expr = Const of rat | UnOp of unary_op * expr | BinOp of binary_op * expr * expr

let rec gcd a b =
  let abs_a = abs a in
  let abs_b = abs b in
  if abs_b = 0 then abs_a
  else gcd abs_b (abs_a mod abs_b)

let reduce (a, b) = let x = a / gcd a b in let y = b / gcd a b in
  if x < 0 && y < 0 then (abs x, abs y) else (x, y)

let add (a, b) (x, y) = reduce (a * y + x * b, b * y)
let sub (a, b) (x, y) = reduce (a * y - x * b, b * y)
let mul (a, b) (x, y) = reduce (a * x, b * y)
let div (a, b) (x, y) = reduce (a * y, b * x)
let neg (a, b) = ((- a), b)

let test = (BinOp (Mul, BinOp (Sub, Const (3,8), Const (2,4)), Const (6,-3)))

let rec eval_expr e = match e with 
  Const (a, b) -> (a, b) |
  UnOp (Neg, e) -> neg (eval_expr e) |
  BinOp (op, e1, e2) -> match op with 
    Add -> add (eval_expr e1) (eval_expr e2) |
    Sub -> sub (eval_expr e1) (eval_expr e2) |
    Mul -> mul (eval_expr e1) (eval_expr e2) | 
    Div -> div (eval_expr e1) (eval_expr e2)
