module TP

open System
open FSharp.Data

type Stock = CsvProvider<"stocks.csv">

[<EntryPoint>]
let main argv =
    let x = Stock.Load("stocks.csv")
    let r = Seq.head x.Rows
    printfn "%A" r.High
    0 // return an integer exit code
