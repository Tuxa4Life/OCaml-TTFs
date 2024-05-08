module type Pairs = sig 
  type 'a pair = 'a * 'a
  val pair: 'a * 'b -> 'a * 'b
  val first: 'a * 'b -> 'a
  val second: 'a * 'b -> 'b
end

module Pairs = struct
  type 'a pair = 'a * 'a
  let pair (a, b) = (a, b)
  let first (a, b) = a
  let second (a, b) = b
end

module type Triples = sig 
  type 'a triple = Triple of 'a * 'a * 'a
  val first: 'a triple -> 'a
  val second: 'a triple -> 'a
  val third: 'a triple -> 'a
end

module Pairs2 = struct 
  type 'a pair = bool -> 'a 
  let pair (a, b) = fun x -> if x then a else b
  let first ab = ab true 
  let second ab = ab false 
end

module Triples = struct 
  type 'a triple = Triple of 'a * 'a * 'a
  let first (Triple (a, _, _)) = a
  let second (Triple (_, b, _)) = b
  let third (Triple (_, _, c)) = c
end