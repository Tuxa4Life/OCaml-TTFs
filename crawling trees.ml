type tree = Leaf | Node of int * tree * tree
type command = Left | Right | Up | New of int | Delete | Push | Pop

let t_l = Node (2, Node (1, Leaf, Leaf), Node (3, Leaf, Leaf))
let t_r = Node (6, Node (5, Leaf, Leaf), Node (7, Leaf, Leaf))
let tree = Node (4, t_l , t_r)

let comms = [Left; Right; Up; Left; Up; Up; New 3];;