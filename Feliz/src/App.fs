module App

open Feliz
open Feliz.Bulma
open Elmish
open Thoth.Json
open System

type Person = {
    First : string
    Last : string
    Alias : string option
}

type Model = {
    Count: int
    People : Person list
    Sort : bool option
}

type Msg =
    | Increment
    | Decrement
    | Load
    | Sort of bool option
    | LoadHandler of Result<Person list, string>
    | Exn of Exception

let init() =
    let model = {
        Count = 0
        People = []
        Sort = None
        }
    model, Cmd.none

let fetchPeople model =
    let decoder = Decode.Auto.generateDecoder<Person list> ()
    let p () =
        promise {
            let people = Decode.fromString decoder TestData.data
            return people
        }
    model, Cmd.OfPromise.either p () LoadHandler Exn

let private toggleSortOrder (x : bool option) =
    if x.IsNone then
        Some true
    else if x.Value then
        Some false
    else
        None

let handleSort model order =
    match order with
    | Some up ->
        let people' =
            if up then
                List.sort model.People
            else
                List.sortDescending model.People
        { model with
            People = people'
            Sort = toggleSortOrder model.Sort
        } , Cmd.none
    | None -> fetchPeople { model with Sort = None }

let update (msg: Msg) (model: Model) =
    match msg with
    | Increment -> { model with Count = model.Count + 1 }, Cmd.none
    | Decrement -> { model with Count = model.Count - 1 }, Cmd.none
    | Load -> fetchPeople model
    | LoadHandler x ->
        match x with
        | Ok p -> { model with People = p }, Cmd.none
        | Error err -> printfn "ERROR: %A" err; model, Cmd.none
    | Exn exn -> printfn "EXN: %A" exn; model, Cmd.none
    | Sort x -> handleSort model x

let render (model: Model) (dispatch: Msg -> unit) =
    Bulma.container [
        Bulma.title3 ("Count is " + string model.Count)
        Bulma.button [
            button.isSuccess
            prop.onClick (fun _ -> dispatch Increment)
            prop.text "Increment"
            prop.style [ style.marginRight 10 ]
        ]

        Bulma.button [
            button.isPrimary
            prop.onClick (fun _ -> dispatch Decrement)
            prop.text "Decrement"
        ]
        Html.hr []
        Bulma.title4 (Interop.Hello.hello "People")
        Bulma.button [
            button.isInfo
            prop.onClick (fun _ -> dispatch Load)
            prop.text "Load"
            if model.People.Length > 0 then
                prop.disabled true
            else
                button.isActive
        ]
        Bulma.table [
            table.isFullwidth
            table.isHoverable
            table.isStriped
            prop.children [
                Html.thead [
                    Html.tr [
                        Html.th [
                            prop.text "First"
                            prop.onClick (fun _ ->
                                dispatch (Sort (toggleSortOrder model.Sort)))
                        ]
                        Html.th "Last"
                        Html.th "Alias"
                    ]
                ]
                Html.tbody [
                    for i in model.People do
                        Html.tr [
                            Html.td i.First
                            Html.td i.Last
                            Html.td (Option.defaultValue "" i.Alias)
                        ]
                ]
            ]
        ]

    ]
