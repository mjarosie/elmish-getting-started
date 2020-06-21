module App

open Elmish
open Elmish.React
open Feliz
open System

type TodoElement =
    { Id: Guid
      Description: string
      Completed: bool
      Edited: bool
      EditedDescription: string option }

type Filter =
    | All
    | Completed
    | NotCompleted

type State =
    { TodoList: TodoElement list
      NewTodo: string
      Filter: Filter }

type Msg =
    | SetNewTodo of string
    | AddNewTodo
    | DeleteTodoElement of Guid
    | ToggleTodoElementCompleted of Guid
    | CancelEdit of Guid
    | ApplyEdit of Guid
    | StartEditingTodoElement of Guid
    | SetEditedDescription of Guid * string
    | SetFilterType of Filter

let init (): State =
    { TodoList =
          [ { Id = Guid.NewGuid()
              Description = "Learn F#"
              Completed = false
              Edited = false
              EditedDescription = None } ]
      NewTodo = ""
      Filter = All }

let update msg state =
    match msg with
    | SetNewTodo newTodo -> { state with NewTodo = newTodo }
    | AddNewTodo when state.NewTodo = "" -> state
    | AddNewTodo ->
        { TodoList =
              { Id = Guid.NewGuid()
                Description = state.NewTodo
                Completed = false
                Edited = false
                EditedDescription = None }
              :: state.TodoList
          NewTodo = ""
          Filter = All }
    | DeleteTodoElement guid ->
        { state with
              TodoList = List.filter (fun x -> x.Id <> guid) state.TodoList }
    | ToggleTodoElementCompleted guid ->
        let newTodoList =
            List.map (fun (x: TodoElement) -> if x.Id = guid then { x with Completed = not (x.Completed) } else x)
                state.TodoList

        { state with TodoList = newTodoList }
    | CancelEdit guid ->
        let newTodoList =
            List.map (fun x ->
                if x.Id = guid then
                    { x with
                          Edited = false
                          EditedDescription = None }
                else
                    x) state.TodoList

        { state with TodoList = newTodoList }
    | ApplyEdit guid ->
        let elementToApplyChanges =
            List.tryFind (fun x -> x.Id = guid) state.TodoList

        match elementToApplyChanges with
        | Some el when el.EditedDescription
                       <> None
                       && el.EditedDescription.Value <> "" ->
            let newTodoList =
                List.map (fun x ->
                    if x.Id = guid then
                        { x with
                              Description = el.EditedDescription.Value
                              Edited = false
                              EditedDescription = None }
                    else
                        x) state.TodoList

            { state with TodoList = newTodoList }
        | _ -> state
    | StartEditingTodoElement id ->
        let newTodoList =
            List.map (fun (el: TodoElement) ->
                if el.Id = id then
                    { el with
                          Edited = true
                          EditedDescription = Some el.Description }
                else
                    el) state.TodoList

        { state with TodoList = newTodoList }
    | SetEditedDescription (id, descr) ->
        let newTodoList =
            List.map (fun (el: TodoElement) ->
                if el.Id = id then
                    { el with
                          EditedDescription = Some descr }
                else
                    el) state.TodoList

        { state with TodoList = newTodoList }
    | SetFilterType filter -> { state with Filter = filter }

open Zanaptak.TypedCssClasses

type Bulma = CssClasses<"https://cdnjs.cloudflare.com/ajax/libs/bulma/0.7.4/css/bulma.min.css", Naming.PascalCase>
type FA = CssClasses<"https://stackpath.bootstrapcdn.com/font-awesome/4.7.0/css/font-awesome.min.css", Naming.PascalCase>

let appTitle =
    Html.p
        [ prop.className Bulma.Title
          prop.text "Elmish To-Do List" ]

let inputField (state: State) (dispatch: Msg -> unit) =
    Html.div
        [ prop.classes [ Bulma.Field; Bulma.HasAddons ]
          prop.children
              [ Html.div
                  [ prop.classes [ Bulma.Control; Bulma.IsExpanded ]
                    prop.children
                        [ Html.input
                            [ prop.classes [ Bulma.Input; Bulma.IsMedium ]
                              prop.valueOrDefault state.NewTodo
                              prop.onChange (SetNewTodo >> dispatch) ] ] ]

                Html.div
                    [ prop.className Bulma.Control
                      prop.children
                          [ Html.button
                              [ prop.classes
                                  [ Bulma.Button
                                    Bulma.IsPrimary
                                    Bulma.IsMedium ]
                                prop.onClick (fun _ -> dispatch AddNewTodo)
                                prop.children
                                    [ Html.span
                                        [ prop.classes [ Bulma.Icon; Bulma.IsMedium ]
                                          prop.children [ Html.i [ prop.classes [ FA.Fa; FA.FaCheck ] ] ] ]
                                      Html.span [ prop.text "Add" ] ] ] ] ] ] ]

/// Helper function to easily construct div with only classes and children
let div (classes: string list) (children: Fable.React.ReactElement list) =
    Html.div
        [ prop.classes classes
          prop.children children ]

let renderTodoElement (todoElem: TodoElement) (dispatch: Msg -> unit) =
    div [ Bulma.Box ]
        [ div
            [ Bulma.Columns
              Bulma.IsMobile
              Bulma.IsVcentered ]
              [ div [ Bulma.Column ]
                    [ Html.p
                        [ prop.className Bulma.Subtitle
                          prop.text todoElem.Description ] ]

                div [ Bulma.Column; Bulma.IsNarrow ]
                    [ div [ Bulma.Buttons ]
                          [ Html.button
                              [ prop.className
                                  [ (true, Bulma.Button)
                                    (todoElem.Completed, Bulma.IsSuccess) ]
                                prop.onClick (fun _ -> dispatch (ToggleTodoElementCompleted todoElem.Id))
                                prop.children [ Html.i [ prop.classes [ FA.Fa; FA.FaCheck ] ] ] ]

                            Html.button
                                [ prop.classes [ Bulma.Button; Bulma.IsPrimary ]
                                  prop.onClick (fun _ -> dispatch (StartEditingTodoElement todoElem.Id))
                                  prop.children [ Html.i [ prop.classes [ FA.Fa; FA.FaEdit ] ] ] ]

                            Html.button
                                [ prop.classes [ Bulma.Button; Bulma.IsDanger ]
                                  prop.onClick (fun _ -> dispatch (DeleteTodoElement todoElem.Id))
                                  prop.children [ Html.i [ prop.classes [ FA.Fa; FA.FaTimes ] ] ] ] ] ] ] ]

let renderEditedTodoElement (elem: TodoElement) (dispatch: Msg -> unit) =
    div [ Bulma.Box ]
        [ div
            [ Bulma.Columns
              Bulma.IsMobile
              Bulma.IsVcentered ]
              [ div [ Bulma.Column ]
                    [ Html.input
                        [ prop.classes [ Bulma.Subtitle; Bulma.IsMedium ]
                          prop.valueOrDefault elem.EditedDescription.Value
                          prop.onTextChange (fun newTxt -> dispatch (SetEditedDescription(elem.Id, newTxt))) ] ]

                div [ Bulma.Column; Bulma.IsNarrow ]
                    [ div [ Bulma.Buttons ]
                          [ Html.button
                              [ prop.className
                                  [ Bulma.Button
                                    if (elem.Description <> elem.EditedDescription.Value)
                                    then Bulma.IsPrimary
                                    else Bulma.IsOutlined ]
                                prop.onClick (fun _ -> dispatch (ApplyEdit elem.Id))
                                prop.children [ Html.i [ prop.classes [ FA.Fa; FA.FaSave ] ] ] ]

                            Html.button
                                [ prop.classes [ Bulma.Button; Bulma.IsWarning ]
                                  prop.onClick (fun _ -> dispatch (CancelEdit elem.Id))
                                  prop.children [ Html.i [ prop.classes [ FA.Fa; FA.FaTimes ] ] ] ] ] ] ] ]

let toDoList (state: State) (dispatch: Msg -> unit) =
    let filteredElements =
        match state.Filter with
        | All -> state.TodoList
        | Completed -> List.filter (fun el -> el.Completed) state.TodoList
        | NotCompleted -> List.filter (fun el -> not el.Completed) state.TodoList

    Html.ul
        [ prop.children
            [ for todo in filteredElements ->
                if todo.Edited then renderEditedTodoElement todo dispatch else renderTodoElement todo dispatch ] ]

let renderFilterChoice (state: State) (dispatch: Msg -> unit): Fable.React.ReactElement =
    div [ Bulma.Buttons; Bulma.HasAddons ]
        [ Html.button
            [ prop.className
                [ (true, Bulma.Button)
                  (state.Filter = Filter.All, Bulma.IsPrimary) ]
              prop.text "All"
              prop.onClick (fun _ -> dispatch (SetFilterType All)) ]
          Html.button
              [ prop.className
                  [ (true, Bulma.Button)
                    (state.Filter = Filter.Completed, Bulma.IsPrimary) ]
                prop.text "Completed"
                prop.onClick (fun _ -> dispatch (SetFilterType Completed)) ]
          Html.button
              [ prop.className
                  [ (true, Bulma.Button)
                    (state.Filter = Filter.NotCompleted, Bulma.IsPrimary) ]
                prop.text "Not Completed"
                prop.onClick (fun _ -> dispatch (SetFilterType NotCompleted)) ] ]

let render (state: State) (dispatch: Msg -> unit) =
    Html.div
        [ prop.style [ style.padding 20 ]
          prop.children
              [ appTitle
                inputField state dispatch
                renderFilterChoice state dispatch
                toDoList state dispatch ] ]

Program.mkSimple init update render
|> Program.withReactSynchronous "elmish-app"
|> Program.run
