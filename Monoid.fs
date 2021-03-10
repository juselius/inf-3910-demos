module Monoid

type OrderLine = {
    Code : string
    Qty : int
    Price : float
}

type Order =
    | Product of OrderLine
    | Total of float
    | Empty
with
    static member Zero = Empty
    static member (+) (a, b) =
        match a, b with
        | Product x, Product y ->
            Total (x.Price * float x.Qty + y.Price * float y.Qty)
        | Total x, Product y -> Total (x + y.Price * float y.Qty)
        | Product x, Total y -> Total (x.Price * float x.Qty + y)
        | Total x, Total y -> Total (x + y)
        | y, Empty -> y
        | Empty, y -> y


let q = { Code = "foo"; Qty = 3; Price = 2.0 }
let order = [ Empty; Total 5.0; Product q; Product q]