(* Assignment 1 *)

module type El_type = sig 
  type t 
  val to_fl : t -> float
end 

module IntEl : (El_type with type t = int) = struct
  type t = int
  let to_fl el = float_of_int el
end

module FloatEl : (El_type with type t = float) = struct
  type t = float 
  let to_fl el = el
end

type rational = int * int

module RatEl : (El_type with type t = rational) = struct 
  type t = rational 
  let to_fl (a, b) = float_of_int a /. float_of_int b
end

module type Mean_type = sig 
  type t 
  val to_float : t -> float
  val mean_init : float * float 
  val mean_aggr : float -> float -> float 
  val mean_op : float -> float -> float 
end

module Mean (Kind : Mean_type) = struct
  type t = Kind.t

  let rec mean = 0
end

module Arith (Elememnt : El_type) : (Mean_type with type t = Elememnt.t) = struct
  type t = Elememnt.t
  let to_float x = Elememnt.to_fl x
  let mean_init = (0., 0.)
  let mean_aggr n x = n +. x
  let mean_op aggr n = aggr /. n 
end

module Geom (Elememnt : El_type) : (Mean_type with type t = Elememnt.t) = struct
  type t = Elememnt.t
  let to_float x = Elememnt.to_fl x
  let mean_init = (0., 0.)
  let mean_aggr n x = n *. x
  let mean_op aggr n = aggr ** (1. /.n) 
end

module Harm (Elememnt : El_type) : (Mean_type with type t = Elememnt.t) = struct
  type t = Elememnt.t
  let to_float x = Elememnt.to_fl x
  let mean_init = (0., 0.)
  let mean_aggr n x = x ** n
  let mean_op aggr n = 0.
end

(* TODO some stuff *)

(* Assignment 2 *)
type 'a onetwo = Null | One of 'a * 'a onetwo | Two of 'a onetwo * 'a * 'a onetwo
let tree = Two (Two (One (0, Null), 1, One (2, Null)), 3, One (4, Null))


let extract_min t = 
  let rec aux t (v, ot) = match t with 
    Null -> (v, ot) | 
    One (x, tree) -> aux tree (Some x, One (x, ot)) | 
    Two (ltree, x, rtree) -> aux ltree (Some x, Two (ot, x, rtree)) 
  in 
  aux t (None, Null)