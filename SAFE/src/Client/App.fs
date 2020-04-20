module Client

open Elmish
open Feliz
open Feliz.Bulma
open Thoth.Json
open Thoth.Fetch
open Shared

type Model = {
    Count: int
    People : Person list
    Sort : bool option
    NewPerson : Person option
}

type Msg =
    | Increment
    | Decrement
    | Load
    | LoadHandler of Result<Person list, string>
    | Save
    | SaveHandler of Result<Person, string>
    | Exn of System.Exception
    | Sort of bool option
    | UpdateFirst of string
    | UpdateLast of string
    | UpdateAlias of string
    | UpdateAge of int
    | UpdateHeight of int

let init() =
    let model = {
        Count = 0
        People = []
        Sort = None
        NewPerson = None
    }
    model, Cmd.none

let fetchPeople model =
    let decoder = Decode.Auto.generateDecoder<Person list> ()
    let p () =
        promise {
            let people = Fetch.tryGet("/api/people", decoder = decoder)
            return! people
        }
    model, Cmd.OfPromise.either p () LoadHandler Exn

let savePerson model =
    let decoder = Decode.Auto.generateDecoder<Person> ()
    match model.NewPerson with
    | Some pers ->
        let p () =
            promise {
                let person =
                    Fetch.tryPost (
                        "/api/person",
                        data = pers,
                        decoder = decoder
                    )
                return! person
            }
        model, Cmd.OfPromise.either p () SaveHandler Exn
    | None ->
        printfn "WARNING: savePerson (): not reached."
        model, Cmd.none

let addPerson model =
    function
    | Ok p ->
        { model with People = p :: model.People }, Cmd.none
    | Error err ->
        printfn "ERROR: addPerson (): %A" err
        model, Cmd.none

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

let updatePerson model (update : Person -> Person) =
    let p = Option.defaultValue Person.New model.NewPerson
    let p' = update p
    let model' = { model with NewPerson = Some p' }
    model', Cmd.none

let updateFirst model s =
    updatePerson model (fun p -> { p with First = s })

let updateLast model s =
    updatePerson model (fun p -> { p with Last = s })

let updateAlias model s =
    updatePerson model (fun p -> { p with Alias = Some s })

let updateAge model n =
    updatePerson model (fun p -> { p with Age = n })

let updateHeight model n =
    updatePerson model (fun p -> { p with Height = n })

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
    | Save -> savePerson model
    | SaveHandler p -> addPerson model p
    | UpdateFirst s -> updateFirst model s
    | UpdateLast s -> updateLast model s
    | UpdateAlias s -> updateAlias model s
    | UpdateAge n -> updateAge model n
    | UpdateHeight n -> updateHeight model n

let addPersonView dispatch =
    let input' ph (msg : string -> Msg) =
        Html.input [
            prop.placeholder ph
            prop.onChange (msg >> dispatch)
        ]
    let iinput' ph (msg : int -> Msg) =
        Html.input [
            prop.type' "number"
            prop.placeholder ph
            prop.onChange (int >> msg >> dispatch)
        ]
    Bulma.box [
        Bulma.columns [
            Bulma.column [ input' "First" UpdateFirst ]
            Bulma.column [ input' "Last" UpdateLast ]
            Bulma.column [ input' "Alias" UpdateAlias ]
            Bulma.column [ iinput' "Age" UpdateAge ]
            Bulma.column [ iinput' "Height" UpdateHeight ]
            Bulma.column [
                Bulma.button [
                    prop.text "Save"
                    button.isDark
                    prop.onClick (fun _ -> dispatch Save)
                ]
            ]
        ]
    ]

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
                       Html.th "Age"
                       Html.th "Height"
                   ]
                ]
                Html.tbody [
                    for i in model.People do
                        Html.tr [
                            Html.td i.First
                            Html.td i.Last
                            Html.td (Option.defaultValue "" i.Alias)
                            Html.td i.Age
                            Html.td i.Height
                        ]
                ]
            ]
        ]
        addPersonView dispatch
    ]
