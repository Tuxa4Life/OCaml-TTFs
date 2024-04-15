(* Fun with Folding *)
let rec length l = match l with 
  [] -> 0 |
  _::xs -> 1 + length xs;;

let longest_list ll = 
  let rec length l = match l with 
  [] -> 0 |
  _::xs -> 1 + length xs in
  let rec aux ll longest = match ll with 
    [] -> longest |
    x::xs -> if length x > length longest then aux xs x else aux xs longest in 
  aux ll [];;

let rec swap l = match l with 
  [] -> [] |
  (k, v)::xs -> (v, k)::swap xs;;

let spread l = 
  let rec aux l acc switch = match l with 
    [] -> acc |
    x::xs -> if switch then x::aux xs acc (not switch) else aux xs (acc @ [x]) (not switch) in 
  aux l [] false;;

let rec find_in_list key l = match l with 
  [] -> failwith "Element with input key was not found." |
  (k, v)::xs -> if k = key then v else find_in_list key xs;;
