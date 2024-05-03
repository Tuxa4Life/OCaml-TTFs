let vector_of (x0, y0) (x1, y1) = (x1 -. x0, y1 -. y0);;
let length (x, y) = x *. y;;
let apply_offset (x, y) (ox, oy) = (x +. ox, y +. oy);;
let scale_length (x, y) l = let current = length (x, y) in 
  let factor = l /. current in (x /. factor, y /. factor);;

(* Exercises *)
let round f = if f -. (floor f) >= 0.5 then ceil f else floor f;;
let seperate f = (floor f, f -. (floor f));;

let concat l = 
  let rec aux l acc = match l with 
  [] -> List.rev acc |
  x::xs -> aux xs (List.rev x @ acc) in 
  aux l [];;

let rec all_true l = match l with 
  [] -> false |
  x::xs -> List.mem true x || all_true xs;;

let count_exclamations s = 
  let count = ref 0 in
  String.iter(fun c -> if c = '!' then count := !count + 1) s; !count;;

let calm_voice s = String.map (fun c -> if c = '!' then '.' else c) s;;
let concat l = String.concat "" l;;