module Live

let f x =  x + 1

let g x = string x + "!"

let h x y = if y  = 0.0 then None else Some (float (f x) / y)

let a = g (h (f 1) (float (f 2)))

let inline (|>.) x f = f x
let inline (.<|) f x = f x

let b = f 2 |>. float |>. h (f 1) |>. g

let inline (>>.) f g x = g (f x)
let inline (.<<) f g x = f (g x)

let fb = f >>. float >>. h (f 1) >>. g
let fb' = g .<< h (f 1) .<< float .<< f

let c = fb 2
let c' = fb' 2

type Outcome<'a> = 
    | Success of 'a
    | Failure of string
with
    static member map f =
        function
        | Success x -> Success (f x)
        | Failure x as y -> y
    static member mapFailure f = 
        function
        | Failure x -> f x |> Failure
        | x -> x

let r1 = Outcome<_>.map f (Success 2)
let r2 : Outcome<unit> = Outcome<_>.mapFailure (fun x -> x + "?") (Failure "err")
