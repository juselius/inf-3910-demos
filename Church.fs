module Chruch

open System

let const' a _ = a

let zero f x = x

let one f x = f x

let two f x = f (f x)

let three f x = f (f (f x))

let rec Nat n f x = if n = 0 then x else f (Nat (n - 1) f x)

let T a b = a

let F a b = b

let pair x y f = f x y

let fst p = p T

let snd p = p F

let ifthenelse p a b = p a b

let succ n f x = f (n f x)

let pred n f x =
    let f1 g h = h (g f)
    n f1 (const' x) id

let add1 = (+) 1

let test' () =
    (Nat 10) (fun x -> x + 2) 0

let test () =
    printfn "Hello World from F#!"
    [
        one add1 1
        Nat 41 add1 1
        fst (pair 2 3)
        snd (pair 2 3)
        ifthenelse T 1 2
        succ three add1 0
        pred three add1 0
    ] |> List.iter (printfn "%d")
    0 // return an integer exit code
