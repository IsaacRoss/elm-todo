port module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, keyCode, onInput, onCheck, onClick)
import Json.Decode as Decode
import Json.Encode


type alias Todo =
    { title : String
    , completed : Bool
    , editing : Bool
    , identifier : Int
    }


type FilterState
    = All
    | Active
    | Completed


type alias Model =
    { todos : List Todo
    , todo : Todo
    , filter : FilterState
    , nextIdentifier : Int
    }


type Msg
    = Add
    | Complete Todo
    | Delete Todo
    | UpdateField String
    | Filter FilterState
    | Clear
    | SetModel Model
    | NoOp


initialModel : Model
initialModel =
    { todos =
        [ { title = "the first todo"
          , completed = False
          , editing = False
          , identifier = 1
          }
        ]
    , todo = { newTodo | identifier = 2 }
    , filter = All
    , nextIdentifier = 3
    }


newTodo : Todo
newTodo =
    { title = ""
    , completed = False
    , editing = False
    , identifier = 0
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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

        Delete todo ->
            let
                newModel =
                    { model | todos = List.filter (\mappedTodo -> todo.identifier /= mappedTodo.identifier) model.todos }
            in
                ( newModel
                , sendToStorage newModel
                )

        UpdateField str ->
            let
                todo =
                    model.todo

                updatedTodo =
                    { todo | title = str }

                newModel =
                    { model | todo = updatedTodo }
            in
                ( newModel
                , sendToStorage newModel
                )

        Filter filterState ->
            let
                newModel =
                    { model | filter = filterState }
            in
                ( newModel
                , sendToStorage newModel
                )

        Clear ->
            let
                newModel =
                    { model
                        | todos = List.filter (\todo -> todo.completed == False) model.todos
                    }
            in
                ( newModel
                , sendToStorage newModel
                )

        SetModel newModel ->
            ( newModel
            , Cmd.none
            )

        NoOp ->
            ( model
            , Cmd.none
            )


todoView : Todo -> Html Msg
todoView todo =
    li
        [ classList
            [ ( "completed", todo.completed ) ]
        ]
        [ div
            [ class "view" ]
            [ input
                [ class "toggle"
                , type_ "checkbox"
                , checked todo.completed
                , onCheck (\_ -> Complete todo)
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


onEnter : Msg -> Attribute Msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Decode.succeed msg
            else
                Decode.fail "not the right keycode"
    in
        on "keydown" (keyCode |> Decode.andThen isEnter)


filterItemView : Model -> FilterState -> Html Msg
filterItemView model filterState =
    li []
        [ a
            [ classList
                [ ( "selected", (model.filter == filterState) ) ]
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
                    , onEnter Add
                    , onInput UpdateField
                    ]
                    []
                ]
            , section [ class "main" ]
                [ ul
                    [ class "todo-list" ]
                    (List.map todoView (filteredTodos model))
                ]
            , footer
                [ class "footer" ]
                [ span [ class "todo-count" ]
                    [ strong [] [ text (toString (List.length (List.filter (\todo -> todo.completed == False) model.todos))) ]
                    , text "Items left"
                    ]
                , ul
                    [ class "filters" ]
                    [ filterItemView model All
                    , filterItemView model Active
                    , filterItemView model Completed
                    ]
                , button
                    [ class "clear-completed"
                    , onClick Clear
                    ]
                    [ text "Clear complted" ]
                ]
            ]
        ]


main =
    Html.program
        { init = ( initialModel, Cmd.none )
        , update = update
        , view = view
        , subscriptions = subscriptions
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
    Json.Encode.string (toString filterState)


mapStorageInput : Decode.Value -> Msg
mapStorageInput modelJson =
    case (decodeModel modelJson) of
        Ok model ->
            SetModel model

        Err errorMessage ->
            let
                _ =
                    Debug.log "Error in mapStorageInput:" errorMessage
            in
                NoOp


decodeModel : Decode.Value -> Result String Model
decodeModel modelJson =
    Decode.decodeValue modelDecoder modelJson


modelDecoder : Decode.Decoder Model
modelDecoder =
    Decode.map4 Model
        (Decode.field "todos" (Decode.list todoDecoder))
        (Decode.field "todo" todoDecoder)
        (Decode.field "filter" (Decode.string |> Decode.map filterStateDecoder))
        (Decode.field "nextIdentifier" Decode.int)


todoDecoder : Decode.Decoder Todo
todoDecoder =
    Decode.map4 Todo
        (Decode.field "title" Decode.string)
        (Decode.field "completed" Decode.bool)
        (Decode.field "editing" Decode.bool)
        (Decode.field "identifier" Decode.int)


filterStateDecoder : String -> FilterState
filterStateDecoder string =
    case string of
        "All" ->
            All

        "Active" ->
            Active

        "Completed" ->
            Completed

        _ ->
            let
                _ =
                    Debug.log "filterStateDecoder" <|
                        "Couldn't decode value"
                            ++ string
                            ++ " so defaulting to All"
            in
                All


sendToStorage : Model -> Cmd Msg
sendToStorage model =
    encodeJson model |> storage


port storageInput : (Decode.Value -> msg) -> Sub msg


port storage : Json.Encode.Value -> Cmd msg
