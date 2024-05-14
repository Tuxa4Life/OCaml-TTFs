(* exercises from HW 4.txt *)


let tuple_list = [(0, "Zero"); (1, "One"); (2, "Two"); (3, "Three"); (4, "Four"); (5, "Five")];;
let int_list = [0; 1; 2; 3; 4; 5; 6; 7; 8; 9];;

let f1 acc (a, b) = acc @ [b, a];;
List.fold_left f1 [] tuple_list;;

let f2 acc x = List.rev acc @ [x];;
List.fold_left f2 [] int_list;;

let f3 acc (k,v) = fun x -> if x = k then v else acc x (* from solutions but ???? *)
List.fold_left (fun _ -> 0) int_list

let map_tr f l = 
  let rec aux f l acc = match l with 
    [] -> acc | 
    x::xs -> aux f xs (acc @ [f x]) in 
  aux f l []

let replicate_tr n x =
  let rec aux n x acc = if n < 1 then acc else aux (n - 1) x (x :: acc) in 
  aux n x []
