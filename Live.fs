module Live

#if INTERACTIVE
#r "nuget: FSharpPlus"
#load "monoids.txt.fs"
#endif

open FSharpPlus
open System
open System.Threading

type Cmd<'Msg> = 'Msg list // free monoid

type Program<'Model, 'Msg> = {
    init : unit -> 'Model * Cmd<'Msg>
    update : 'Msg -> 'Model -> 'Model * Cmd<'Msg>
    view : 'Model -> ('Msg -> unit) -> unit
}

let run (program : Program<'Model, 'Msg>) =
    let (model, initCmd) = program.init ()
    let mvu = MailboxProcessor.Start (fun inbox ->
        let rec loop (state : 'Model) =
            async {
                let! msg = inbox.Receive ()
                let state' =
                    try
                        let (model', cmd') = program.update msg state
                        program.view model' inbox.Post
                        cmd' |> List.iter inbox.Post
                        model'
                    with ex ->
                        printfn "Unable to process a message: %A" ex
                        state
                return! loop state'
            }
        loop model
    )
    printfn "starting event loop"
    initCmd |> List.iter mvu.Post

type Model = { sum : int }

type Msg =
    | Increment of int
    | Decrement of int

let init () : Model * Cmd<Msg> =
    { sum = 0 }, [ Increment 0 ]

let update msg model =
    match msg with
    | Increment n ->
        printfn "+ %d" n
        { model with sum = model.sum + n }, []
    | Decrement n ->
        printfn "- %d" n
        { model with sum = model.sum - n }, []

let simEvent dispatch =
    let rnd = Random()
    let x = rnd.NextDouble () * 500.0 |> int
    let y = rnd.NextDouble () * 10.0 |> int
    async {
        Threading.Thread.Sleep x
        if x > 4 then
            dispatch (Increment y)
        else
            dispatch (Decrement y)
    }
    |> Async.Start

let view model dispatch =
    printfn "SUM is %d" model.sum
    simEvent dispatch

let runProgram () =
    {
        init = init
        update = update
        view = view
    } |> run
    Threading.Thread.Sleep Threading.Timeout.Infinite