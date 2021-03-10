module Live

let f x = x + 1

let g x = string x + "!"

let h x y = if y = 0.0 then None else Some (float (f x) / y)

let a = g (h (f 1) (float (f 2)))

let inline (|>.) x f = f x
let inline (.<|) f x = f x

let inline (>>.) f g x = g (f x)
let inline (.<<) f g x = f (g x)

let b = f 2 |>. float |>. h (f 1) |>. g
let b' =  g .<| (h (f 1) .<| (float .<| f 2))

let fb = f >>. float >>. h (f 1) >>. g
let fb' =  g .<< h (f 1) .<< float .<< f

let c  = fb 2
let c' = fb' 2

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