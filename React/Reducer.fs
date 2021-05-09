module App

open System
open Browser.Dom
open Feliz

type Model = { sum: int }

type Msg =
    | Increment of int
    | Decrement of int

let init () : Model = { sum = 0 }

let update model msg =
    match msg with
    | Increment n -> { model with sum = model.sum + n }
    | Decrement n -> { model with sum = model.sum - n }

let simEvent dispatch =
    let rnd = Random()
    let x = rnd.Next 10
    let y = rnd.Next 10
    async {
        Threading.Thread.Sleep (x * 500)
        if x > 4 then
            dispatch (Increment y)
        else
            dispatch (Decrement y)
        return ()
    }
    |> Async.Start

let view model dispatch =
    printfn "SUM is %d" model.sum
    simEvent dispatch

let myCounterx =
    Fable.React.FunctionComponent.Of (fun () ->
        let initialModel = init ()

        let model, dispatch = React.useReducer(update, initialModel)

        React.useEffect ((fun _ -> dispatch (Increment 0)), [||])

        Html.div []
    )

let myCounter =
    Fable.React.FunctionComponent.Of (fun () ->
        let initialModel = init ()

        let model, dispatch = React.useReducer(update, initialModel)

        React.useEffect ((fun _ -> dispatch (Increment 0)), [||])

        Html.div [ myCounterx () ]
    )

ReactDOM.render(myCounter (), document.getElementById "feliz-app")
