let length l = List.fold_left (fun acc _ -> acc + 1) 0 l
let rev l = List.fold_left (fun acc x -> x::acc) [] l
let map f l = List.fold_right (fun x acc -> (f x)::acc) l []
let filter pred l = List.fold_right (fun x acc -> if pred x then x::acc else acc) l []