
let rec interleave l1 l2 l3 = match l1, l2, l3 with 
  x::xs, y::ys, z::zs -> x::y::z::interleave xs ys zs |
  [], [], [] | _::_, _::_, [] |_::_, [], _::_ | [], _::_, _::_ | _::_, [], [] | [], _::_, [] | [], [], _::_ -> [];;
