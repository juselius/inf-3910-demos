module Client

open Elmish
open Feliz
open Feliz.Bulma
open Feliz.Recharts
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
    | Init of Result<Counter, string>
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
    let decoder = Decode.Auto.generateDecoder<Counter> ()
    let p () =
        promise {
            let people = Fetch.tryGet("/api/init", decoder = decoder)
            return! people
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
        printfn "ERROR: addPerson (): %A" err
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
        { model with
            People = p :: model.People
            NewPerson = None
        }, Cmd.none
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

let handleLoad model =
    function
    | Ok p -> { model with People = p }, Cmd.none
    | Error err -> printfn "ERROR: %A" err; model, Cmd.none

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
    | Init c -> handleInit model c
    | Increment -> { model with Count = model.Count + 1 }, Cmd.none
    | Decrement -> { model with Count = model.Count - 1 }, Cmd.none
    | Exn exn -> printfn "EXN: %A" exn; model, Cmd.none
    | Load -> fetchPeople model
    | LoadHandler x -> handleLoad model x
    | Sort x -> handleSort model x
    | Save -> savePerson model
    | SaveHandler p -> addPerson model p
    | UpdateFirst s -> updateFirst model s
    | UpdateLast s -> updateLast model s
    | UpdateAlias s -> updateAlias model s
    | UpdateAge n -> updateAge model n
    | UpdateHeight n -> updateHeight model n

let lineChart model =
    Recharts.lineChart [
        lineChart.width 500
        lineChart.height 300
        lineChart.data model.People
        lineChart.margin(top=5, right=30)
        lineChart.children [
            Recharts.cartesianGrid [ cartesianGrid.strokeDasharray(3, 3) ]
            Recharts.xAxis [ xAxis.dataKey (fun point -> point.First) ]
            Recharts.yAxis []
            Recharts.tooltip []
            Recharts.legend []
            Recharts.line [
                line.monotone
                line.dataKey (fun point -> point.Age)
                line.stroke "#8884d8"
            ]

            Recharts.line [
                line.monotone
                line.dataKey (fun point -> point.Height)
                line.stroke "#82ca9d"
            ]
        ]
    ]

let barChart model =
    Recharts.barChart [
        barChart.width 500
        barChart.height 300
        barChart.data model.People
        barChart.margin(top=5, right=30)
        barChart.children [
            Recharts.cartesianGrid [ cartesianGrid.strokeDasharray(3, 3) ]
            Recharts.xAxis [ xAxis.dataKey (fun point -> point.First) ]
            Recharts.yAxis []
            Recharts.tooltip []
            Recharts.legend []
            Recharts.bar [
                bar.dataKey (fun point -> point.Age)
                bar.fill "#8884d8"
            ]

            Recharts.bar [
                bar.dataKey (fun point -> point.Height)
                bar.fill "#82ca9d"
            ]
        ]
    ]

let chartsView model =
    if model.People.Length > 1 then
        Bulma.columns [
            Bulma.column [ lineChart model ]
            Bulma.column [ barChart model ]
        ]
    else
        Html.div []


let peopleView model dispatch =
    let tbl =
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
    Html.div [
        if model.People.Length > 0 then
            tbl
        else
            Html.div []
    ]

let counterView model dispatch =
    Html.div [
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
        ]
        Bulma.button [
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
        counterView model dispatch
        Bulma.section [
            Bulma.title3 (Interop.Hello.hello "People")
            addPersonView model dispatch
            peopleView model dispatch
        ]
        chartsView model
    ]
