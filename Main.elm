module Main exposing (..)

import Html.Attributes exposing (..)
import Html exposing (..)
import Html.App as App
import Json.Decode as Json
import Html.Events exposing (keyCode, on, onInput, onCheck, onClick)


type alias Todo =
    { title : String
    , completed : Bool
    , editing : Bool
    , identifier : Int
    }


type alias Model =
    { todos : List Todo
    , todo : Todo
    , filter : FilterState
    , nextIdentifier : Int
    }


newTodo : Todo
newTodo =
    { title = ""
    , completed = False
    , editing = False
    , identifier = 0
    }


initialModel : Model
initialModel =
    { todos =
        [ { title = "The first todo"
          , completed = True
          , editing = False
          , identifier = 1
          }
        ]
    , todo = { newTodo | identifier = 2 }
    , nextIdentifier = 3
    , filter = All
    }


type FilterState
    = All
    | Active
    | Completed


type Msg
    = Add
    | Complete Todo
    | Uncomplete Todo
    | Delete Todo
    | Filter FilterState
    | UpdateField String
    | ClearComplete


handleKeyPress : Json.Decoder Msg
handleKeyPress =
    Json.map (always Add) (Json.customDecoder keyCode is13)


is13 : Int -> Result String ()
is13 code =
    if code == 13 then
        Ok ()
    else
        Err "not the right key code"


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateField str ->
            let
                currentTodo =
                    model.todo

                updatedTodo =
                    { currentTodo | title = str }
            in
                { model | todo = updatedTodo }

        Add ->
            { model
                | todos = model.todo :: model.todos
                , todo = { newTodo | identifier = model.nextIdentifier }
                , nextIdentifier = model.nextIdentifier + 1
            }

        Complete todo ->
            let
                updateTodo thisTodo =
                    if thisTodo.identifier == todo.identifier then
                        { todo | completed = True }
                    else
                        thisTodo
            in
                { model
                    | todos = List.map updateTodo model.todos
                }

        Uncomplete todo ->
            let
                updateTodo thisTodo =
                    if thisTodo.identifier == todo.identifier then
                        { todo | completed = False }
                    else
                        thisTodo
            in
                { model | todos = List.map updateTodo model.todos }

        Filter filterState ->
            { model | filter = filterState }

        Delete todo ->
            { model | todos = List.filter (\mappedTodo -> mappedTodo.identifier /= todo.identifier) model.todos }

        ClearComplete ->
            { model | todos = List.filter (\todo -> todo.completed /= True) model.todos }


todoView : Todo -> Html Msg
todoView todo =
    let
        handleComplete =
            case todo.completed of
                True ->
                    (\_ -> Uncomplete todo)

                False ->
                    (\_ -> Complete todo)
    in
        li [ classList [ ( "completed", todo.completed ) ] ]
            [ div [ class "view" ]
                [ input
                    [ class "toggle"
                    , type' "checkbox"
                    , checked todo.completed
                    , onCheck handleComplete
                    ]
                    []
                , label [] [ text todo.title ]
                , button
                    [ class "destroy"
                    , onClick (Delete todo)
                    ]
                    []
                ]
            ]


view : Model -> Html Msg
view model =
    div []
        [ section [ class "todoapp" ]
            [ header [ class "header" ]
                [ h1 [] [ text "todos" ]
                , input
                    [ class "new-todo"
                    , placeholder "What needs to be done?"
                    , autofocus True
                    , value model.todo.title
                    , on "keypress" handleKeyPress
                    , onInput UpdateField
                    ]
                    []
                ]
            , section [ class "main" ]
                [ ul [ class "todo-list" ]
                    (List.map todoView (filteredTodos model))
                ]
            , footer [ class "footer" ]
                [ span [ class "todo-count" ]
                    [ strong []
                        [ model.todos
                            |> List.filter (\todo -> todo.completed == False)
                            |> List.length
                            |> toString
                            |> text
                        ]
                    , text " items left"
                    ]
                , ul [ class "filters" ]
                    [ filterItemView model All
                    , filterItemView model Active
                    , filterItemView model Completed
                    ]
                , button
                    [ class "clear-completed"
                    , onClick ClearComplete
                    ]
                    [ text "Clear Completed" ]
                ]
            ]
        ]


filterItemView : Model -> FilterState -> Html Msg
filterItemView model filterState =
    li []
        [ a
            [ classList [ ( "selected", (model.filter == filterState) ) ]
            , href "#"
            , onClick (Filter filterState)
            ]
            [ text (toString filterState) ]
        ]


filteredTodos : Model -> List Todo
filteredTodos model =
    let
        matchesFilter =
            case model.filter of
                All ->
                    (\_ -> True)

                Active ->
                    (\todo -> todo.completed == False)

                Completed ->
                    (\todo -> todo.completed == True)
    in
        List.filter matchesFilter model.todos


main : Program Never
main =
    App.beginnerProgram
        { model = initialModel
        , update = update
        , view = view
        }
