type 'a llist = Cons of 'a * (unit -> 'a llist)

let rec lnat start = Cons (start, (fun () -> lnat (start + 1))) 

let lfib () =
  let rec aux a b = Cons(a, (fun () -> aux b (a + b))) in 
  aux 0 1 

let rec ltake n ll = if n = 0 then [] else match ll with 
  Cons (x, f) -> x::ltake (n - 1) (f ())

let rec lfilter pred ll = match ll with 
  Cons (x, f) -> if pred x then Cons(x, (fun () -> lfilter pred (f ()))) else lfilter pred (f ())