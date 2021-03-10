module Matching

let (|Foo|Bar|) inp =
    if inp > 3 then Foo else Bar

let (|Boo|) x inp =
    if inp > 3 then (x,2,3) else (0,0,0)

let f x =
    match x with
    | Boo 1 (a,b,c) -> printfn "%A %A" 1 a


let (|Integer|_|) (str: string) =
   let mutable intvalue = 0
   if System.Int32.TryParse(str, &intvalue) then Some(intvalue)
   else None

let (|Float|_|) (str: string) =
   let mutable floatvalue = 0.0
   if System.Double.TryParse(str, &floatvalue) then Some(floatvalue)
   else None

let parseNumeric str =
   match str with
     | Integer i -> printfn "%d : Integer" i
     | Float f -> printfn "%f : Floating point" f
     | _ -> printfn "%s : Not matched." str

parseNumeric "1.1"
parseNumeric "0"
parseNumeric "0.0"
parseNumeric "10"
parseNumeric "Something else"


type ABC = {
    A : int
    B : float
    C : string
}

let aOrB ({ A = x } as t) = if x < 0 then t.B else float x

let (|Pairup|) x = (x, x + 1)

let oof (Pairup (x,y)) = x + y