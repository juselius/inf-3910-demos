module Fun

let f x = x * x

let f' = fun x -> x * x


let g x y = x * y

let g' x = fun y -> x * y

let g'' = fun x -> fun y -> x * y

let g''' = fun x y -> x * y

let g'''' = (*)


let h = g 2

let v = h 21


let apply f x = f x

let a1 = apply f 5

let a2 = apply (g 2) 21


let twice f x = f (f x)

let t1 = twice f 2

let t2 = twice (g 2)

let t2' = twice h 2


let curry f x y = f (x, y)

let gPair (x, y) = x * y

let gFun x y = curry gPair x y

let gFun' = curry gPair

let c1 = gPair (1,2)

let c2 = gFun' 1 2

let c3 = c1 = c2

let uncurry f (x, y) = f x y

let gPair' = uncurry gFun

let p1 = gPair' (1, 2)


let lines (x : string) = x.Split [|'\n'|] |> List.ofArray
let unlines x = List.fold (fun a s -> a + s + "\n") "" x

let sortLines s     = unlines (List.sort   (lines s))
let revLines s      = unlines (List.rev    (lines s))
let twoFirstLines s = unlines (List.take 2 (lines s))

let byLines f x = unlines (f (lines x))

let sortLines'     = byLines  List.sort
let revLines'      = byLines  List.rev
let twoFirstLines' = byLines (List.take 2)

let poem = System.IO.File.ReadAllText "siphonaptera.txt"

printfn "%s\n" poem
printfn "%s\n" (sortLines' poem)
printfn "%s\n" (revLines' poem)
printfn "%s\n" (twoFirstLines' poem)