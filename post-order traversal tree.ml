type 'a tree = Lf | Br of 'a * 'a tree * 'a tree;;

let rec list_of_tree_post t = match t with 
    Lf -> [] | 
    Br (x, l, r) -> (list_of_tree_post l) @ (list_of_tree_post r) @ [x];;
