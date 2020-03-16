module CPS

let inline p x = printfn "%A" x

type Cont<'r, 'a> = Cont of (('a -> 'r) -> 'r)

// constant cps
let five f = f 5
five string |> p

// alt constant
let five' = (|>) 5
five' string |> p

// weird and wonderful
List.map ((|>) 2) [ (*) 2; (*) 3; (+) 42] |> p

// pythagoras direct
let add = (+)
let square x = x * x
let pythagoras x y = add (square x) (square x)
pythagoras 2 3 |> p

// pythagoras cps
let addCPS x y = fun f -> f (x + y)
let squareCPS x f =  f (x * x)
let pythagorasCPS x y f =
  squareCPS x (fun x' ->
  squareCPS y (fun y' ->
  addCPS x' y' f))
pythagorasCPS 2 3 p

// thrice
let thrice f x
   = x |> f |> f |> f
thrice (add 2) 3 |> p

// thrice cps
let thriceCPS (f : 'a -> ('a -> 'r) -> 'r) (x : 'a) =
  fun k ->
    f x (fun fx ->
    f fx ( fun ffx ->
    f ffx k))
thriceCPS (addCPS 2) 3 p

