module Applicative

let rec apply (f : ('a -> 'b) list) (x : 'a list) =
    match f with
    | [] -> []
    | f' :: fx -> List.map f' x @ apply fx x

let (<*>) = apply

let pure' x = [x]

let (<%>) = List.map

let f x y z = x + y + z

let g = pure' f <*> [1..2] <*> [2..3] <*> [4..5]
let g' = f <%> [1..2] <*> [2..3] <*> [4..5]