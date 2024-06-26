let lt_seq_but_incorrect l = 
  let key t = match t with (k, v) -> k in 
  let value t = match t with (k, v) -> v in
  let rec aux l prev curr = if value curr > value prev then aux l curr curr else
    match l with 
    [] -> prev |
    x::xs -> if x <> key curr then aux xs prev (x, 1) else aux xs prev (x, (value curr) + 1) in
  let rec t_to_list t = match t with 
    (k, v) -> if v = 0 then [] else k::t_to_list (k, v - 1) in
  t_to_list (aux l (-1, 1) (-1, 1));;


let rec devide_list l n c = match l with 
  [] -> [] |
  x::xs -> if c mod n = 0 then x::[] :: devide_list xs n (c + 1) else x::devide_list xs n (c + 1)