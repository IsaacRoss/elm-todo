port module Main exposing (..)

import Html.Attributes exposing (..)
import Html exposing (..)
import Html.App as App
import Json.Decode as Json
import Html.Events exposing (keyCode, on, onInput, onCheck, onClick)
import Json.Decode exposing ((:=))
import Json.Encode


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
    | SetModel Model
    | NoOp


handleKeyPress : Json.Decoder Msg
handleKeyPress =
    Json.map (always Add) (Json.customDecoder keyCode is13)


is13 : Int -> Result String ()
is13 code =
    if code == 13 then
        Ok ()
    else
        Err "not the right key code"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateField str ->
            let
                currentTodo =
                    model.todo

                updatedTodo =
                    { currentTodo | title = str }

                newModel =
                    { model | todo = updatedTodo }
            in
                ( newModel, sendToStorage newModel )

        Add ->
            let
                newModel =
                    { model
                        | todos = model.todo :: model.todos
                        , todo = { newTodo | identifier = model.nextIdentifier }
                        , nextIdentifier = model.nextIdentifier + 1
                    }
            in
                ( newModel
                , sendToStorage newModel
                )

        Complete todo ->
            let
                updateTodo thisTodo =
                    if thisTodo.identifier == todo.identifier then
                        { todo | completed = True }
                    else
                        thisTodo

                newModel =
                    { model
                        | todos = List.map updateTodo model.todos
                    }
            in
                ( newModel
                , sendToStorage newModel
                )

        Uncomplete todo ->
            let
                updateTodo thisTodo =
                    if thisTodo.identifier == todo.identifier then
                        { todo | completed = False }
                    else
                        thisTodo
            in
                ( { model | todos = List.map updateTodo model.todos }, Cmd.none )

        Filter filterState ->
            ( { model | filter = filterState }, Cmd.none )

        Delete todo ->
            ( { model | todos = List.filter (\mappedTodo -> mappedTodo.identifier /= todo.identifier) model.todos }, Cmd.none )

        ClearComplete ->
            ( { model | todos = List.filter (\todo -> todo.completed /= True) model.todos }, Cmd.none )

        SetModel newModel ->
            newModel ! []

        NoOp ->
            model ! []


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
    App.program
        { init = ( initialModel, Cmd.none )
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    storageInput mapStorageInput


encodeJson : Model -> Json.Encode.Value
encodeJson model =
    Json.Encode.object
        [ ( "todos", Json.Encode.list (List.map encodeTodo model.todos) )
        , ( "todo", encodeTodo model.todo )
        , ( "filter", encodeFilterState model.filter )
        , ( "nextIdentifier", Json.Encode.int model.nextIdentifier )
        ]


encodeTodo : Todo -> Json.Encode.Value
encodeTodo todo =
    Json.Encode.object
        [ ( "title", Json.Encode.string todo.title )
        , ( "completed", Json.Encode.bool todo.completed )
        , ( "editing", Json.Encode.bool todo.editing )
        , ( "identifier", Json.Encode.int todo.identifier )
        ]


encodeFilterState : FilterState -> Json.Encode.Value
encodeFilterState filterState =
    case filterState of
        All ->
            Json.Encode.string "All"

        Active ->
            Json.Encode.string "Active"

        Completed ->
            Json.Encode.string "Completed"


mapStorageInput : Json.Decode.Value -> Msg
mapStorageInput modelJson =
    case (decodeModel modelJson) of
        Ok model ->
            SetModel model

        _ ->
            NoOp


decodeModel : Json.Decode.Value -> Result String Model
decodeModel modelJson =
    Json.Decode.decodeValue modelDecoder modelJson


modelDecoder : Json.Decode.Decoder Model
modelDecoder =
    Json.Decode.object4 Model
        ("todos" := Json.Decode.list todoDecoder)
        ("todo" := todoDecoder)
        ("filter" := filterStateDecoder)
        ("nextIdentifier" := Json.Decode.int)


todoDecoder : Json.Decode.Decoder Todo
todoDecoder =
    Json.Decode.object4 Todo
        ("title" := Json.Decode.string)
        ("completed" := Json.Decode.bool)
        ("editing" := Json.Decode.bool)
        ("identifier" := Json.Decode.int)


filterStateDecoder : Json.Decode.Decoder FilterState
filterStateDecoder =
    let
        decodeToFilterState string =
            case string of
                "All" ->
                    Result.Ok All

                "Active" ->
                    Result.Ok Active

                "Completed" ->
                    Result.Ok Completed

                _ ->
                    Result.Err ("Not a valid filterState: " ++ string)
    in
        Json.Decode.customDecoder Json.Decode.string decodeToFilterState


sendToStorage : Model -> Cmd Msg
sendToStorage model =
    encodeJson model |> storage


port storageInput : (Json.Decode.Value -> msg) -> Sub msg


port storage : Json.Encode.Value -> Cmd msg
