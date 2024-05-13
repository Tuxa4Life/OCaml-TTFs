(* sexercises from  HW 3.txt *)

let rec member f t l = match l with 
  [] -> false | 
  x::xs -> f t x = 0 || member f t xs;;        

let count_occurrences l =  
  let rec sort l = 
    let rec insert  (a, b) l = match l with 
      [] -> [(a, b)] | 
      (x, y)::xs -> if b >= y then (a, b)::l else (x, y)::insert (a, b) xs in 
    match l with 
      [] -> [] |
      (x, y)::xs -> insert (x, y) (sort xs) in 
  let rec increase a l = match l with 
    [] -> [(a, 1)] |
    (x, y)::xs -> if x = a then (x, y + 1)::xs else (x, y)::increase a xs in 
  let rec aux l acc = match l with 
    [] -> sort acc |
    x::xs -> aux xs (increase x acc) in 
  aux l [];;

let rec drop_last l = match l with 
    [] -> failwith "Empty list has no last element" |
    [x] -> [] |
    x::xs -> x::drop_last xs;;

let rec drop_last_opt l = 
  let rec aux l acc = match l with 
    [] -> None | 
    [x] -> Some (List.rev acc) | 
    x::xs -> aux xs (x::acc) in 
  aux l [];;

let rec zip_with f l1 l2 = match l1, l2 with 
  l, [] -> [] | [], l -> [] |
  x::xs, y::ys -> (f x y)::zip_with f xs ys;;

let rec unzip l = 
  let rec get_keys l  = match l with 
    [] -> [] |
    (a, _)::xs -> a::get_keys xs in 
  let rec get_values l  = match l with 
    [] -> [] |
    (_, b)::xs -> b::get_values xs in 
  (get_keys l, get_values l);;


  type team = Arg | Sau | Mex | Pol | Por | Kor | Uru | Gha

  let table_and_scorers l = 
    let create_table l = 
      let rec length = function 
          [] -> 0 | 
          _::xs -> 1 + length xs in 
                 
      let rec game_count team l = match l with 
          [] -> 0 | 
          (t1, _, t2, _)::xs -> if t1 = team || t2 = team then 1 + game_count team xs else game_count team xs in
            
      let rec win_count team l = match l with 
          [] -> 0 |
          (t1, scrs1, t2, scrs2)::xs -> if (t1 = team && length scrs1 > length scrs2) || (t2 = team && length scrs1 < length scrs2) then 1 + win_count team xs else win_count team xs in
        
      let rec draw_count team l = match l with 
          [] -> 0 |
          (t1, scrs1, t2, scrs2)::xs -> if (t1 = team || t2 = team) && length scrs1 = length scrs2 then 1 + draw_count team xs else draw_count team xs in
            
      let rec lose_count team l = match l with 
          [] -> 0 |
          (t1, scrs1, t2, scrs2)::xs -> if (t1 = team && length scrs1 < length scrs2) || (t2 = team && length scrs1 > length scrs2) then 1 + lose_count team xs else lose_count team xs in
                
      let rec goals_count team l = match l with 
          [] -> 0 | 
          (t1, scrs1, t2, scrs2)::xs -> if team = t1 then length scrs1 + goals_count team xs else if team = t2 then length scrs2 + goals_count team xs else goals_count team xs in 
    
      let rec conc_goals_count team l = match l with 
          [] -> 0 | 
          (t1, scrs1, t2, scrs2)::xs -> if team = t1 then length scrs2 + conc_goals_count team xs else if team = t2 then length scrs1 + conc_goals_count team xs else conc_goals_count team xs in
  
      let point_count team l = 3 * win_count team l + draw_count team l in
    
      let rec member x l = match l with 
          [] -> false |
          h::t -> if h = x then true else member x t in
      let comp (_, _, _, _, _, gf, ga, p)  (_, _, _, _, _, gf1, ga1, p1) = if p - p1 > 0  then 1 else if p - p1 < 0 then -1 else if gf - ga > gf1 - ga1 then 1 else if gf - ga < gf1 - ga1 then - 1 else if gf > gf1 then 1 else -1 in
      let rec insert a l = match l with 
          [] -> [a] |
          x::xs -> if comp a x > 0 then a::l else x::insert a xs in
      let rec sort l = match l with 
          [] -> [] |
          x::xs -> insert x (sort xs) in
  
      let rec build_table l left checked = match left with 
          [] -> [] |
          (team, _, team2, _)::xs -> if member team checked then if member team2 checked then build_table l xs checked else (team2, game_count team2 l, win_count team2 l, draw_count team2 l, lose_count team2 l, goals_count team2 l, conc_goals_count team2 l, point_count team2 l)::build_table l xs (team::checked) else (team, game_count team l, win_count team l, draw_count team l, lose_count team l, goals_count team l, conc_goals_count team l, point_count team l)::build_table l xs (team::checked) in
      sort (build_table l l []) in 
      
    let create_scorers l = 
      let rec input_scorer (n, t) l = match l with 
          [] -> [(n, t, 1)] |
          (name, team, goals)::xs -> if name = n then (name, team, goals + 1)::xs else (name, team, goals)::input_scorer (n, t) xs
      in let rec get_tuples (t1, s1, t2, s2) = match s1, s2 with 
            [], [] -> [] | x::xs, [] -> (x, t1)::get_tuples (t1, xs, t2, s2) | [], x::xs -> (x, t2)::get_tuples (t1, s1, t2, xs) |
            x::xs, y::ys -> (x, t1)::(y, t2)::get_tuples (t1, xs, t2, ys)
      in let rec get_all_tuples l = match l with 
            [] -> [] |
            x::xs -> get_tuples x @ get_all_tuples xs
                 
      in let rec insert (a, b, c) l = match l with 
            [] -> [(a, b, c)] |
            (x, y, z)::xs -> if c > z then (a, b, c)::l else (x, y, z)::insert (a, b, c) xs
      in let rec sort l = match l with 
            [] -> [] |
            x::xs -> insert x (sort xs)
      in let rec build_scorers l acc = match l with 
            [] -> acc |
            (n, t)::xs -> build_scorers xs (input_scorer (n, t) acc) 
      in sort (build_scorers (get_all_tuples l) []) in 
    
    (create_table l, create_scorers l) 
  
