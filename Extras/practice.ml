let dict = [(0, "Zero"); (1, "One"); (2, "Two"); (3, "Three"); (4, "Four"); (5, "Five"); (6, "Six"); (7, "Seven");]

let print_dict_entry (k, v) = 
  print_string "> Key: "; print_int k; print_string " | Value: \"" ; print_string v; print_string "\"" ;
  print_newline ();;

let rec forEach f l = match l with 
  [] -> () |
  x::xs -> f x; forEach f xs;;

let print_dict = forEach print_dict_entry;;

let rec read_dict () = 
  try 
    print_string "< Key : ";
    let k = read_int () in match k with 
      0 -> [] |
      x -> print_string "| Value: "; let v = read_line() in (k, v)::read_dict()
  with Failure string_of_int -> print_string "Key value must be an integer"; print_newline(); read_dict();;

let entry_to_channel ch (k, v) =
  output_string ch "> Value: "; 
  output_string ch (string_of_int k);
  output_string ch " | Key: \"";
  output_string ch v;
  output_string ch "\"";
  output_char ch '\n';;

let dict_to_channel ch d = forEach (entry_to_channel ch) d;;

let dict_to_file filename d = 
  let ch = open_out filename in
    dict_to_channel ch d;
  close_out ch;;

let entry_of_channel ch = 
  let name = input_line ch in name;;

let rec dictionary_of_channel ch = 
  try 
    let e = entry_of_channel ch in
    e::dictionary_of_channel ch
  with End_of_file -> [];;

let file_to_dict filename =
  let ch = open_in filename in 
  let dict = dictionary_of_channel ch in
  close_in ch; dict;;


  (* Exercises *)
let print_list lst =
  let rec iter l = match l with 
    [] -> () |
    [x] -> print_string (string_of_int x); print_string "]" |
    x::xs -> print_string (string_of_int x) ; print_string "; "; iter xs in

  print_string "["; 
  iter lst; 
  print_newline();;

let rec create_tuple () = 
  try
    let first = read_int () in 
    let second = read_int () in 
    let third = read_int () in (first, second, third)
  with Failure string_of_int -> print_string "Inputs must be integers" ; create_tuple();;

let table filename n = 
  let rec line f n m = if (n + 1) = m then "" else 
    string_of_int (f m) ^ "\t" ^ line f n (m + 1) in
  let rec create n m acc = if (n + 1) = m then acc else 
    create n (m + 1) (((line (fun x -> m * x) n 1) ^ "\n")::acc) in
  let rec rev l = match l with 
    [] -> [] |
    x::xs -> (rev xs) @ [x] in

  let ch = open_out filename in 
    forEach (output_string ch) (rev (create n 1 [])) ;
  close_out ch;;

let file_lines filename = 
  let rec counter ch = try let _ = input_line ch in 1 + counter ch with End_of_file -> 0 in 
  let ch = open_in filename in let a = counter ch in close_in ch; a;;

let copy org cp = 
  let rec get_lines ch =
    try let line = input_line ch in
      line::get_lines ch 
    with End_of_file -> [] in 
    
  let ch = open_in org in 
  let data = get_lines ch in 
  close_in ch;
  let ch1 = open_out cp in 
    forEach (output_string ch1) data;
  close_out ch1;;