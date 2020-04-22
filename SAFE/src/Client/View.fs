module View

open Feliz
open Feliz.Bulma
open Feliz.Recharts
open Shared
open Client

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