module Functors

let f x = x + 1

let g x = string x + "!"

type Outcome<'a> =
    | Success of 'a
    | Failure of string
with
    static member map f =
        function
        | Success x -> Success (f x)
        | (Failure x) as y -> y
    static member mapFailure f =
        function
        | Failure x -> f x |> Failure
        | x -> x
    static member apply f x =
        match f with
        | Success f' -> Outcome<_>.map f' x
        | Failure x -> Failure x
    static member pure = Success

let r1 : Outcome<int> = Outcome<_>.map f (Success 4)
let r2 : Outcome<int> = Outcome<_>.mapFailure g (Failure "err")

let (<*>) = Outcome<_>.apply

// let r3 : Outcome<float option> =
let r3 =
    Success f <*> Success 1

let r3' =
    Success f <*> Failure "err"

// let r4 =
//     let h' = Success h
//     h' <*> Success (f 1) <*> Success 1.0