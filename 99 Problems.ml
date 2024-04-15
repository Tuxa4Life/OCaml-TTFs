(* 1. Write a function last : 'a list -> 'a option that returns the last element of a list. (easy) *)
let rec last l = match l with 
  [] -> None |
  [x] -> Some x |
  x::xs -> last xs;;

(* 2. Find the last but one (last and penultimate) elements of a list. (easy) *)
let rec last_two l = match l with 
  [] -> None | [x] -> None |
  [x; y] -> Some (x, y) |
  _::xs -> last_two xs;;

(* 3. Find the K'th element of a list. (easy) *)
let rec at i l = match l with 
  [] -> None |
  x::xs -> if i = 0 then Some x else at (i - 1) xs;;

(* 4. Find the number of elements of a list. (easy) *)
let rec length l = match l with
 [] -> 0 |
 _::xs -> 1 + length xs;;

(* 5. Reverse a list. (easy) *)
let rec reverse l = match l with 
  [] -> [] |
  x::xs -> (reverse xs) @ [x];;

(* 6. Find out whether a list is a palindrome. (easy) *)
let rec is_palindrome l = l = reverse l;; (* used function: line 24 *)

(* TODO: *)  (* 7. Flatten a nested list structure. (medium) *)

(* TODO: *)  (* 8. Eliminate consecutive duplicates of list elements. (medium) *)
