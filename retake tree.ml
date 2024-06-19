type 'a tlist = Node of ('a * 'a tlist) list;;

(* example tree *)
let t0 = Node [(1, Node [(2, Node []); (3, Node [])]); (5, Node [])];;
let t1 = Node [(3, Node [(1, Node []); (2, Node []); (7, Node [])]); (5, Node [(4, Node [(0, Node []); (9, Node [])]); (6, Node [])])]
let t2 = Node [(1, Node [(4, Node[]); (3, Node [])]); (2, Node [])];;


(* Start of 1 *)
let rec get_min t = match t with (* chemi strategia: calke min vipovo, calke amovadzro, ertad davabruno *)
  Node [] -> max_int | 
  Node ((v, s)::xs) -> min (min v (get_min s)) (get_min (Node xs));;

let join t1 t2 = match t1, t2 with 
  Node [], Node [] -> Node [] |
  t1, Node [] -> t1 | Node [], t2 -> t2 |
  Node (x::xs), Node (y::ys) -> Node ([x; y;] @ xs @ ys) 

let rec extract t = match t with (* es extract igive remove aris chatvalet, ar vicodi rom strictly sorted iqneboda tree *)
  Node [] -> Node [] |
  Node ((v, s)::xs) -> if v = get_min t then match s with 
    Node [] -> Node xs |
    Node ((v1, s1)::xs1) -> Node ((v1, (join s1 (Node xs1)))::xs) 
  else 
    let Node same_level = extract (Node xs) in (* aq tu ver gaigebt agixsnit calke kinda crazy shit *)
    Node ((v, extract s)::same_level);;

let extract_min t = (Some (get_min t), extract t) (* wina funqciebs viyeneb da pirdapir vabruneb *)
(* End of 1 *)


(* Start of 2 *)
let rec only_values t = match t with 
  Node [] -> [] |
  Node ((v, s)::xs) -> v::only_values (Node xs);;

let rec verify t = match t with (* tito layers vaqcev listad, da igive sorted listze vamowmeb da booleanebs &&bit vabav *)
  Node [] -> true | 
  Node ((v, s)::xs) -> (only_values t = List.sort compare (only_values t)) && verify s && verify (Node xs);;
(* End of 2 *)


(* Start of 3 *)
let rec insert (value, sub) t = match t with  (* calke insert davwere rom mivyve lists da vainserto da vainserto *)
  Node [] -> Node [(value, sub)] |
  Node ((v, s)::xs) -> if value < v then Node ((v, insert (value, sub) s)::xs) 
    else 
      let Node insert_on_level = insert (value, sub) (Node xs) in (* aq tu ver gaigebt agixsnit calke kinda crazy shit *)
      Node ((v, s)::insert_on_level);;

let rec from_list l = match l with 
  [] -> Node [] |
  x::xs -> insert (x, Node []) (from_list xs);;
(* End of 3 *)


(* Start of 4 *)
let rec layer_to_list t = match t with 
  Node [] -> [] |
  Node ((v, s)::xs) -> v::only_values (Node xs);;

let rec find_in_list a l = match l with 
  [] -> false |
  x::xs -> if x = a then true else find_in_list a xs;;

let rec find x t = match t with 
  Node [] -> false |
  Node ((v, s)::xs) -> find_in_list x (layer_to_list t) || find_in_list x (layer_to_list s) || find_in_list x (layer_to_list (Node xs));;

let rec remove_el x t = match t with 
  Node [] -> Node [] |
  Node ((v, s)::xs) -> if x = v then match s with 
    Node [] -> Node xs |
    Node ((v1, s1)::xs1) -> Node ((v1, (join s1 (Node xs1)))::xs)  (* igive extract_minis logikaa aqac *)
  else 
    let Node remove_on_level = remove_el x (Node xs) in
    Node ((v, remove_el x s)::remove_on_level);;

let remove x t = ((find x t), remove_el x t)
(* End of 4 *)
