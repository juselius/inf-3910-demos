module CPS

let inline p x = printfn "%A" x

// constant cps
let five f = f 5
five string |> p

// alt constant
let five' = (|>) 5
five' string |> p

let theValueFive = five id

let two = (|>) 2

// weird and wonderful
List.map two [ (*) 2; (*) 3; (+) 42] |> p

// pythagoras direct
let add = (+)
let square x = x * x
let pythagoras x y = add (square x) (square y)
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

// chain cps
let chain
  (s : ('a -> 'r) -> 'r)
  (f : ('a -> ('b -> 'r) -> 'r))
  = fun k -> s (fun x -> f x k)

// bind
let (>>=) = chain

// thrice bind
let thriceM f x = (|>) x >>= f >>= f >>= f
thriceM (addCPS 2) 3 p

type Cont<'r, 'a> = Cont of (('a -> 'r) -> 'r)

// Cont<r,a> functor
module Cont =
    let map f x = fun g -> x (f >> g)
    let ret x = fun f -> f x

// cont builder
type ContBuilder() =
  member x.Bind(a, b) = a >>= b
  member x.Return a = Cont.ret a
  member x.ReturnFrom a = a

let cont = ContBuilder()

// cont CE
let thriceCE f x =
    cont {
        let! x' = f x
        let! x'' = f x'
        return! f x''
    }
thriceCE (addCPS 2) 3 p
