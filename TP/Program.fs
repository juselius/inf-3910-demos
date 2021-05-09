module TP

open System
open FSharp.Data

type Stocks =  CsvProvider<"stocks.csv">

[<EntryPoint>]
let main argv =

    let stocks = Stocks.Load("stocks.csv")
    Seq.iter (printfn "%A") stocks.Rows 
    0 // return an integer exit code
