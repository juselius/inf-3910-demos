module Composition

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