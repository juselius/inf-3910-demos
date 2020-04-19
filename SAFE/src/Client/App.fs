module App

open Elmish
open Feliz
open Feliz.Bulma
open Thoth.Json
open Shared

type Model = {
    Count: int
    People : Person list
    Sort : bool option
}

type Msg =
    | Increment
    | Decrement
    | Load
    | LoadHandler of Result<Person list, string>
    | Exn of System.Exception
    | Sort of bool option

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

let update (msg: Msg) (model : Model) =
    match msg with
    | Increment -> { model with Count = model.Count + 1 }, Cmd.none
    | Decrement -> { model with Count = model.Count - 1 }, Cmd.none
    | Exn exn -> printfn "EXN: %A" exn; model, Cmd.none
    | Load -> fetchPeople model
    | LoadHandler x ->
        match x with
        | Ok p -> { model with People = p }, Cmd.none
        | Error err -> printfn "ERROR: %A" err; model, Cmd.none
    | Sort x -> handleSort model x

let render (model: Model) (dispatch: Msg -> unit) =
    Bulma.container [
        Bulma.title3 ("Strike counter: " + string model.Count)
        Bulma.button [
            button.isSuccess
            prop.style [ style.marginRight 7 ]
            prop.onClick (fun _ -> dispatch Increment)
            prop.text "Increment"
        ]
        Bulma.button [
            button.isPrimary
            prop.onClick (fun _ -> dispatch Decrement)
            prop.text "Decrement"
        ]
        Html.hr []
        Bulma.title3 (Interop.Hello.hello "People")
        Bulma.button [
            if model.People.Length > 0 then prop.disabled true else button.isInfo
            prop.style [ style.marginRight 7 ]
            prop.onClick (fun _ -> dispatch Load)
            prop.text "Load"
        ]
        Bulma.table [
            table.isFullwidth
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
