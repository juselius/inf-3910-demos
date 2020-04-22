module Client

open Elmish
open Feliz
open Feliz.Bulma
open Feliz.Recharts
open Thoth.Json
open Thoth.Fetch
open Shared

type Msg =
    | Init of Result<Counter, string>
    | Increment
    | Decrement
    | Load
    | LoadHandler of Result<Person list, string>
    | Save
    | SaveHandler of Result<PersonId, string>
    | Exn of System.Exception
    | Sort of bool option
    | UpdateFirst of string
    | UpdateLast of string
    | UpdateAlias of string
    | UpdateAge of int
    | UpdateHeight of int
    | Edit of Person
    | StartEdit of Result<PersonId * Person, string>
    | Delete of Person
    | DoDelete of Result<PersonId * Person, string>
    | Update


let init () =
    let decoder = Decode.Auto.generateDecoder<Counter> ()
    let p () =
        promise {
            return! Fetch.tryGet("/api/init", decoder = decoder)
        }
    let model = {
        Count = 0
        People = []
        Sort = None
        NewPerson = None
    }
    model, Cmd.OfPromise.either p () Init Exn

let handleInit model =
    function
    | Ok counter ->
        { model with Count = counter.Value }, Cmd.none
    | Error err ->
        printfn "ERROR: handleInit (): %A" err
        model, Cmd.none

let savePerson model =
    let decoder = Decode.Auto.generateDecoder<PersonId> ()
    match model.NewPerson with
    | Some (_, pers) ->
        let p () =
            promise {
                return! Fetch.tryPost (
                    "/api/person",
                    data = pers,
                    decoder = decoder
                )
            }
        model, Cmd.OfPromise.either p () SaveHandler Exn
    | None ->
        printfn "WARNING: savePerson (): not reached."
        model, Cmd.none

let updatePerson model =
    let decoder = Decode.Auto.generateDecoder<PersonId> ()
    match model.NewPerson with
    | Some (pId, pers) ->
        let p () =
            promise {
                return! Fetch.tryPut (
                    "/api/person",
                    data = (pId, pers),
                    decoder = decoder
                )
            }
        model, Cmd.OfPromise.either p () SaveHandler Exn
    | None ->
        printfn "WARNING: updatePerson (): not reached."
        model, Cmd.none

let commitPerson model =
    function
    | Ok _ ->
        if model.NewPerson.IsSome then
            { model with
                People = (snd model.NewPerson.Value) :: model.People
                NewPerson = None
            }, Cmd.none
        else
            model, Cmd.none
    | Error err ->
        printfn "ERROR: savePerson: %A" err
        model, Cmd.none

let fetchPeople model =
    let decoder = Decode.Auto.generateDecoder<Person list> ()
    let p () =
        promise {
            let people = Fetch.tryGet("/api/people", decoder = decoder)
            return! people
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

let setPerson model (update : Person -> Person) =
    let (pId, p) = Option.defaultValue (0, Person.New) model.NewPerson
    let p' = update p
    let model' = { model with NewPerson = Some (pId, p') }
    model', Cmd.none

let handleLoad model =
    function
    | Ok p -> { model with People = p }, Cmd.none
    | Error err -> printfn "ERROR: %A" err; model, Cmd.none

let updateFirst model (s : string) =
    setPerson model (fun p -> { p with First = s.Trim() })

let updateLast model (s : string) =
    setPerson model (fun p -> { p with Last = s.Trim() })

let updateAlias model (s : string) =
    setPerson model (fun p -> { p with Alias = Some (s.Trim()) })

let updateAge model n =
    setPerson model (fun p -> { p with Age = n })

let updateHeight model n =
    setPerson model (fun p -> { p with Height = n })

type Promp = (unit -> Fable.Core.JS.Promise<Result<(PersonId * Person),string>>) ->  Cmd<Msg>

let getPerson (model : Model) (person : Person) (cmd : Promp) =
    let decoder = Decode.Auto.generateDecoder<PersonId * Person> ()
    let req = sprintf "/api/person/%s/%s" person.First person.Last
    let p () =
        promise {
            return! Fetch.tryGet(req, decoder = decoder)
        }
    model, cmd p

let handleEdit model (person : Person) =
    getPerson model person (fun p -> Cmd.OfPromise.either p () StartEdit Exn)

let handleStartEdit model p =
    match p with
    | Ok person ->
        let model' =
            { model with
                NewPerson = Some person
                People =
                    model.People
                    |> List.filter ((<>) (snd person) )
        }
        model', Cmd.none
    | Error err ->
        printfn "ERROR: handleStartEdit: %s" err
        model, Cmd.none

let handleDelete model (person : Person) =
    getPerson model person (fun p -> Cmd.OfPromise.either p () DoDelete Exn)

let handleCommitDelete model p =
    let decoder = Decode.Auto.generateDecoder<unit> ()
    match p with
    | Ok (pId, person) ->
        let p () =
            promise {
              return! Fetch.delete ("/api/person", pId, decoder = decoder)
            }
        let people = model.People |> List.filter ((<>) person)
        let model' = { model with People = people }
        model', Cmd.OfPromise.attempt p () Exn
    | Error err ->
        printfn "ERROR: handleCommitDelete: %s" err
        model, Cmd.none

let update (msg: Msg) (model : Model) =
    match msg with
    | Init c -> handleInit model c
    | Increment -> { model with Count = model.Count + 1 }, Cmd.none
    | Decrement -> { model with Count = model.Count - 1 }, Cmd.none
    | Exn exn -> printfn "EXN: %A" exn; model, Cmd.none
    | Load -> fetchPeople model
    | LoadHandler x -> handleLoad model x
    | Sort x -> handleSort model x
    | Save -> savePerson model
    | SaveHandler p -> commitPerson model p
    | UpdateFirst s -> updateFirst model s
    | UpdateLast s -> updateLast model s
    | UpdateAlias s -> updateAlias model s
    | UpdateAge n -> updateAge model n
    | UpdateHeight n -> updateHeight model n
    | Edit p -> handleEdit model p
    | StartEdit p -> handleStartEdit model p
    | Delete p -> handleDelete model p
    | DoDelete p -> handleCommitDelete model p
    | Update -> updatePerson model

let peopleView model dispatch =
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
                   Html.th [
                        prop.width 5
                        prop.text "Edit"
                   ]
                   Html.th [
                        prop.width 5
                        prop.text "Remove"
                   ]
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
                        Html.td [
                            Bulma.button [
                                button.isLight
                                prop.onClick (fun _ -> dispatch (Edit i))
                                prop.children [
                                    Html.div [ prop.className "fa fa-edit"]
                                ]
                            ]
                        ]
                        Html.td [
                            Bulma.button [
                                button.isLight
                                prop.onClick (fun _ -> dispatch (Delete i))
                                prop.children [
                                    Html.div [ prop.className "fa fa-trash"]
                                ]
                            ]
                        ]
                    ]
            ]
        ]
    ]

let counterView model dispatch =
    Bulma.box [
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
    ]

let addPersonView model dispatch =
    let input' ph (msg : string -> Msg) (value : string) =
        Html.input [
            prop.placeholder ph
            prop.onChange (msg >> dispatch)
            prop.value value
        ]
    let iinput' ph (msg : int -> Msg) (value : int) =
        Html.input [
            prop.type' "number"
            prop.placeholder ph
            prop.onChange (int >> msg >> dispatch)
            prop.value value
        ]
    let (pId, p) = Option.defaultValue (0, Person.New) model.NewPerson
    Bulma.box [
        Bulma.columns [
            Bulma.column [ input' "First" UpdateFirst p.First ]
            Bulma.column [ input' "Last" UpdateLast p.Last ]
            Bulma.column [ input' "Alias" UpdateAlias (Option.defaultValue "" p.Alias)]
            Bulma.column [ iinput' "Age" UpdateAge p.Age ]
            Bulma.column [ iinput' "Height" UpdateHeight p.Height ]
        ]
        Bulma.button [
            if pId > 0 then
                prop.text "Update"
                prop.style [ style.marginRight 8 ]
                button.isDark
                prop.onClick (fun _ -> dispatch Update)
            else
                prop.text "Save"
                prop.style [ style.marginRight 8 ]
                button.isDark
                prop.onClick (fun _ -> dispatch Save)
            if model.NewPerson.IsNone then
                prop.disabled true
            else
                prop.disabled false
        ]
        Bulma.button [
            if model.People.Length > 0 then
                prop.disabled true
            else
                button.isInfo
            prop.style [ style.marginRight 7 ]
            prop.onClick (fun _ -> dispatch Load)
            prop.text "Load"
        ]
    ]

let render (model: Model) (dispatch: Msg -> unit) =
    Bulma.container [
        Bulma.section [ counterView model dispatch ]
        Bulma.section [
            Bulma.title3 (Interop.Hello.hello "People")
            addPersonView model dispatch
            if model.People.Length > 0 then
                Bulma.box [
                    peopleView model dispatch
                    Charts.chartsView model
                ]
        ]
    ]
