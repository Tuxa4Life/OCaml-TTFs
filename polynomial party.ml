let eval_poly x l = 
  let rec length l = match l with 
    [] -> 0 |
    _::xs -> 1 + length xs in
  let rec pow x n = if n <= 0 then failwith "Positive argument needed" else 
    match n with 
      1 -> x |
      _ -> x * pow x (n - 1) in 
  let rec aux x l n = match l with 
    [] -> 0 |
    h::t -> (h * pow x n) + aux x t (n - 1) in 
  aux x l (length l);;