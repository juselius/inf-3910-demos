module Poetry

open System.IO

let lines (s : string) = s.Split [|'\n'|] |> List.ofArray

let unlines (sl : string list) =
    sl |> List.fold (fun a s -> a + s + "\n") ""

let sortLines'' s = unlines (List.sort (lines s))
let sortLines''' s = s |> lines |> List.sort |> unlines
let sortLines'''' = lines >> List.sort >> unlines

let sortLines s = s |> lines |> List.sort |> unlines
let revLines s = s |> lines |> List.rev |> unlines
let twoLines s = s |> lines |> List.take 2 |> unlines

let byLines f s = s |> lines |> f |> unlines

let sortLines' = byLines List.sort
let revLines' = byLines List.rev
let twoLines' = byLines (List.take 2)

let indent s = "   " + s
let indent' = (+) "   "

let indentEachLine = byLines (List.map indent)

let eachLine f s = s |> lines |>  List.map f |> unlines
let eachLine' f = lines >>  List.map f >> unlines
let eachLine'' f = byLines (List.map f)

let indentEachLine' = eachLine indent

let private yell (s : string) = s.ToUpper () + "!!! "

let yellEachLine = eachLine yell

let words (s : string) = s.Split [|' '|] |> List.ofArray

let unwords = List.fold (+) ""

let eachWord f = words >> List.map f >> unwords

let yellEachWord = eachWord yell

let eachWordOnEachLine f = eachLine (eachWord f)

let yellEachWordOnEachLine = eachWordOnEachLine yell

let poem = File.ReadAllText "siphonaptera.txt"

let sortedPoem = sortLines poem

let recite () =
    printfn "%s" (revLines poem)
    printfn "%s" (poem |> revLines |> twoLines)
    printfn "%s" (poem |> indentEachLine)
    printfn "%s" (poem |> yellEachLine)
    printfn "%s" (poem |> yellEachWord)
    printfn "%s" (poem |> yellEachWordOnEachLine |> indentEachLine)