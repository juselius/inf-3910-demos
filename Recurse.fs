module Recurse

let rec fact n =
    if n = 0 then 1 else n * fact (n - 1)

let factTCO n =
    let rec f acc x =
        if x = 0 then acc else f (x * acc) (x - 1)
    f 1 n

let rec f x = if x > 0 then g x else x
and g x = if x > 0 then f (x - 1) else x - 1


type Folder = {
    Name : string
    Contents : Contents
}
and Contents =
    | Folder of Folder
    | File of {| Name : string; Data : byte array |}

type List'<'a> =
    | Cons of 'a * List'<'a>
    | Nil

let (^+) x xs = Cons (x, xs)

let l = Cons (1, Cons (2, Cons (3, Nil)))

let l' = 1 ^+ 2 ^+ 3 ^+ Nil

let rec sum n = function
    | Cons (x, xs) -> sum (n + x) xs
    | Nil -> n

