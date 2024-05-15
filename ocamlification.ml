let rec foo x y b = if x > y then foo y x b else
  match x < y with 
    true -> if b then foo (x + 1) y (not b) else foo x (y - 1) (not b) |
    false -> x;;