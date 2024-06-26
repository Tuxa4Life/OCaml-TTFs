type 'a onetwo = Null | One of 'a * 'a onetwo | Two of 'a onetwo * 'a * 'a onetwo ;;

let example_tree = Two (One (1, Null), 2, Two (One (3, Null), 4, Two (Null, 5, One(6, Null))));;
let binary = Two (Two (Null, 4, Null), 2, Two (Null, 3, Null));;

let list = [5; 2; 6; 7; 2; 1];;

let extract_min t = 
  let rec find_min t n = match t with 
      Null -> n | 
      Two (t1, x, t2) -> find_min t1 x | 
      One (x, t1) -> match t1 with 
        One (y, t2) -> if x > y then find_min t1 x else x | 
        _ -> find_min t1 x in
  let min = find_min t max_int in 

  let rec save_tree t = match t with 
      Null -> Null |
      Two (t1, x, t2) -> if x = min then Null else Two (save_tree t1, x, t2) |
      One (x, t1) -> match t1 with 
        One (y, t2) -> if x < y && x = min then t1 else One (x, save_tree t1) |
        _ -> if x = min then Null else One (x, save_tree t1)
    
    in (Some min, save_tree t);; 




let check_strictness t = 
    let compare_left x t1 = match t1 with 
    Null -> true |
    One (y, _) | Two (_, y, _) -> x > y in

    let compare_right x t1 = match t1 with 
        Null -> true |
        One (y, _) | Two (_, y, _) -> x < y in

    let rec verify tr side = match tr with 
        Null -> true | 
        One (x, t) -> if side then (compare_right x t) && verify t side else (compare_left x t) && verify t side |
        Two (t1, x, t2) -> (compare_left x t1) && (compare_right x t2) && (verify t1 false) && verify t2 true in
    verify t false


let rec insert x t = match t with 
    Null -> Two (Null, x, Null) | 
    One (y, t1) -> if x <> y then One (y, insert x t1) else t | 
    Two (t1, y, t2) -> if x < y then Two (insert x t1, y, t2) else if x > y then Two (t1, y, insert x t2) else t;;

let rec to_normal t = match t with 
    Null -> Null |
    Two (t1, x, t2) -> Two (to_normal t1, x, to_normal t2) | 
    One (x, t1) -> match t1 with 
      Null -> Two (Null, x, Null) |
      Two (t11, y, t22) -> if x > y then Two (t11, y, to_normal (insert x t22)) else Two (to_normal (insert x t11), y, t22) | 
      One (y, t1) -> if x > y then Two (insert y Null, x, to_normal t1) else Two (insert x Null, y, to_normal t1);;
  
let rec from_list l = match l with
    [] -> Null | 
    x::xs -> insert x (from_list xs);;

let rec find x t = match t with 
    Null -> false | 
    One (y, t1) -> if y = x then true else find x t1 |
    Two (t1, y, t2) -> if y = x then true else find x t1 || find x t2;;

let rec remove x t = (* ik this removes whole tree down from target element without saving *)
  let rec aux x t = match t with 
      Null -> Null |
      One (x1, t1) -> if x1 = x then Null else One (x1, aux x t1) |
      Two (t1, x1, t2) -> if x1 = x then Null else Two (aux x t1, x1, aux x t2) in 
  
  (find x t), (aux x t);;
    



