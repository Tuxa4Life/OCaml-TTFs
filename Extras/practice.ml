let print_stats lines chars words sents = 
  print_string "Amount of lines: ";
  print_int !lines;
  print_newline();
  print_string "Amount of characters: ";
  print_int !chars;
  print_newline();
  print_string "Amount of words: ";
  print_int !words;
  print_newline();
  print_string "Amount of sentences: ";
  print_int !sents;
  print_newline();;

let print_histograms arr = 
  print_string "--- List of Character frequencies ---";
  print_newline();

  for x = 0 to 255 do 
    if arr.(x) > 0 then begin
      print_string "[";
      print_char (char_of_int x); print_string "]: ";
      print_int arr.(x); print_string " ";
    end
  done;;

let channel_statistics in_channel = 
  let lines = ref 0 in 
  let characters = ref 0 in 
  let words = ref 0 in 
  let sentences = ref 0 in
  let histogram = Array.make 256 0 in
  try 
    while true do 
      let line = input_line in_channel in 
      lines := !lines + 1;
      characters := !characters + String.length line;
      String.iter (fun c -> 
        match c with 
          '.' | '?' | '!' -> sentences := !sentences + 1 |
          ' ' -> words := !words + 1 |
          _ -> ()
      ) line;
      String.iter (fun c -> histogram.((int_of_char c)) <- histogram.((int_of_char c)) + 1) line
    done
  with 
    End_of_file -> 
      print_stats lines characters words sentences;
      print_newline();
      print_histograms histogram;;
      
let file_statistics name = 
  let ch = open_in name in 
  try 
    channel_statistics ch;
    close_in ch;
with _ -> close_in ch;;


(* Exercises *)
let sum arr = 
  let output = ref 0 in
  for i = 0 to (Array.length arr) - 1 do 
    output := !output + arr.(i)
  done; 
  !output;;

let reverse arr = 
  let length = (Array.length arr) - 1 in
  for i = 0 to (length / 2) do 
      let tmp = arr.(i) in
      arr.(i) <- arr.(length - i);
      arr.(length - i) <- tmp
  done;
  arr;;

let array_table n = 
  let output = Array.make n [||] in 
  for x = 0 to n - 1 do 
    output.(x) <- Array.make n 0
  done;
  for x = 0 to (n - 1) do 
    for y = 0 to (n - 1) do
      output.(y).(x) <- (x + 1) * (y + 1)
    done 
  done;
  output;;

let to_uppercase s = 
  let output = ref "" in 
  String.iter (fun c -> 
    let num = int_of_char c in 
    if num >= 97 && num <= 122 then 
      output := !output ^ (String.make 1 (char_of_int (num - 32)))
    else output := !output ^ String.make 1 c
  ) s; output;;

let to_lowercase s = 
  let output = ref "" in 
  String.iter (fun c -> 
    let num = int_of_char c in 
    if num >= 65 && num <= 90 then 
      output := !output ^ (String.make 1 (char_of_int (num + 32)))
    else output := !output ^ String.make 1 c
  ) s; output;;
