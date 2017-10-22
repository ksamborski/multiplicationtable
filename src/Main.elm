module Main exposing (..)

import Array exposing (Array)
import Bootstrap.Alert as Alert
import Bootstrap.Button as Button
import Bootstrap.CDN as CDN
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Form.InputGroup as InputGroup
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Grid
import Bootstrap.Table as Table
import Date exposing (Date)
import Date.Extra.Duration as Date
import Html exposing (Html)
import Html.Attributes as Html
import Maybe
import Random
import Task


type alias Model =
    { table : Array (Array (Maybe Bool))
    , currentMult : Maybe CurrentMult
    , time : Maybe ( Date, Maybe Date )
    , errorCount : Int
    , lastCorrect : Maybe Bool
    }


type alias CurrentMult =
    { x : Int
    , y : Int
    , input : String
    }


type Action
    = StartGame
    | GameStartDate Date
    | GameEndDate Date
    | RandomPick Int
    | Input String
    | CheckResult Int


initTable : Int -> Array (Array (Maybe Bool))
initTable size =
    Array.repeat size (Array.repeat size Nothing)


initModel : Int -> Model
initModel size =
    { table = initTable size
    , currentMult = Nothing
    , time = Nothing
    , errorCount = 0
    , lastCorrect = Nothing
    }


main : Program Never Model Action
main =
    Html.program
        { init = ( initModel 10, Cmd.none )
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }


availableMult : Array (Array (Maybe Bool)) -> Array ( Int, Int )
availableMult a =
    Array.fromList <|
        List.foldl
            (\( idx, a2 ) lst2 ->
                List.append
                    (List.foldl
                        (\( y, v ) lst ->
                            if not <| Maybe.withDefault False v then
                                ( idx + 1, y + 1 ) :: lst
                            else
                                lst
                        )
                        []
                     <|
                        Array.toIndexedList a2
                    )
                    lst2
            )
            []
        <|
            Array.toIndexedList a


availableMultCount : Array (Array (Maybe Bool)) -> Int
availableMultCount a =
    Array.foldl (\a2 c -> c + (Array.length <| Array.filter (not << Maybe.withDefault False) a2)) 0 a


doneCount : Array (Array (Maybe Bool)) -> Int
doneCount a =
    Array.foldl (\a2 c -> c + (Array.length <| Array.filter (Maybe.withDefault False) a2)) 0 a


wrongCount : Array (Array (Maybe Bool)) -> Int
wrongCount a =
    Array.foldl (\a2 c -> c + (Array.length <| Array.filter (not << Maybe.withDefault True) a2)) 0 a


update : Action -> Model -> ( Model, Cmd Action )
update act m =
    let
        randomPick tbl =
            Random.generate RandomPick <|
                Random.int 0 <|
                    max (availableMultCount tbl - 1) 0
    in
    case act of
        StartGame ->
            ( { m | table = initTable 10, errorCount = 0, lastCorrect = Nothing }
            , Task.perform GameStartDate Date.now
            )

        GameStartDate d ->
            ( { m | time = Just ( d, Nothing ) }
            , randomPick m.table
            )

        GameEndDate d ->
            case m.time of
                Just ( sd, _ ) ->
                    ( { m | time = Just ( sd, Just d ), currentMult = Nothing }
                    , Cmd.none
                    )

                Nothing ->
                    ( m, Cmd.none )

        RandomPick idx ->
            case Array.get idx (availableMult m.table) of
                Just ( x, y ) ->
                    ( { m | currentMult = Just { x = x, y = y, input = "" } }, Cmd.none )

                Nothing ->
                    ( m, Task.perform GameEndDate Date.now )

        Input s ->
            case m.currentMult of
                Just cm ->
                    ( { m | currentMult = Just { cm | input = s } }, Cmd.none )

                Nothing ->
                    ( m, randomPick m.table )

        CheckResult i ->
            case m.currentMult of
                Just cm ->
                    let
                        ntbl =
                            case Array.get (cm.x - 1) m.table of
                                Just a ->
                                    Array.set
                                        (cm.x - 1)
                                        (Array.set (cm.y - 1) (Just (cm.x * cm.y == i)) a)
                                        m.table

                                Nothing ->
                                    m.table
                    in
                    ( { m
                        | table = ntbl
                        , lastCorrect =
                            if cm.x * cm.y == i then
                                Just True
                            else
                                Just False
                        , errorCount =
                            if cm.x * cm.y == i then
                                m.errorCount
                            else
                                m.errorCount + 1
                      }
                    , randomPick ntbl
                    )

                Nothing ->
                    ( m, Cmd.none )


view : Model -> Html Action
view m =
    Grid.containerFluid []
        [ CDN.stylesheet
        , viewGame m
        , viewLastStatus m
        , viewTable m.table
        ]


viewTable : Array (Array (Maybe Bool)) -> Html Action
viewTable tbl =
    let
        center =
            Table.cellAttr (Html.style [ ( "text-align", "center" ) ])

        row x a =
            Table.tr [] <|
                Table.th [ center ] [ Html.text <| toString x ]
                    :: (List.map
                            (\( y, v ) ->
                                case v of
                                    Just True ->
                                        Table.td [ center ] [ Html.text <| toString <| (y + 1) * x ]

                                    Just False ->
                                        Table.td [ center, Table.cellDanger ] []

                                    _ ->
                                        Table.td [ center ] []
                            )
                        <|
                            Array.toIndexedList a
                       )
    in
    Table.table
        { options = [ Table.bordered, Table.hover ]
        , thead =
            Table.simpleThead <|
                Table.th
                    [ center ]
                    [ Html.text "x" ]
                    :: (List.map (\x -> Table.th [ center ] [ Html.text <| toString x ]) <|
                            List.range 1 (Array.length tbl)
                       )
        , tbody =
            Table.tbody [] <|
                List.map
                    (\( idx, a ) -> row (idx + 1) a)
                    (Array.toIndexedList tbl)
        }


viewGame : Model -> Html Action
viewGame m =
    Grid.row [ Grid.centerXs, Grid.attrs [ Html.style [ ( "padding", "100px" ) ] ] ] <|
        case m.currentMult of
            Just cm ->
                [ Grid.col [ Col.attrs [ Html.style [ ( "text-align", "center" ) ] ], Col.xs8 ]
                    [ Form.formInline
                        [ Html.style [ ( "width", "fit-content" ), ( "margin", "auto" ) ] ]
                        [ Form.label
                            [ Html.for "wynik" ]
                            [ Html.text <|
                                toString cm.x
                                    ++ " • "
                                    ++ toString cm.y
                                    ++ " = "
                            ]
                        , InputGroup.config
                            (InputGroup.text
                                [ Input.id "wynik"
                                , Input.placeholder "Wpisz wynik"
                                , Input.onInput Input
                                , Input.value cm.input
                                ]
                            )
                            |> InputGroup.successors
                                [ InputGroup.button
                                    (Button.success
                                        :: (case Result.toMaybe (String.toInt cm.input) of
                                                Just i ->
                                                    [ Button.onClick (CheckResult i) ]

                                                Nothing ->
                                                    [ Button.disabled True ]
                                           )
                                    )
                                    [ Html.text "Sprawdź" ]
                                ]
                            |> InputGroup.view
                        ]
                    ]
                , Grid.col [ Col.attrs [ Html.style [ ( "text-align", "center" ) ] ], Col.xs4 ]
                    [ Html.text <| "Poprawnych odpowiedzi: " ++ toString (doneCount m.table)
                    , Html.br [] []
                    , Html.text <| "Błędnych odpowiedzi: " ++ toString (wrongCount m.table)
                    , Html.br [] []
                    , Html.text <| "Pozostało do zrobienia: " ++ toString (availableMultCount m.table)
                    ]
                ]

            Nothing ->
                case m.time of
                    Just ( sd, Just ed ) ->
                        let
                            ddiff =
                                Date.diff ed sd
                        in
                        [ Grid.col [ Col.attrs [ Html.style [ ( "text-align", "center" ) ] ] ]
                            [ Html.h2 [] [ Html.text "Brawo!" ]
                            , Html.text <|
                                "Ukończyłeś grę w "
                                    ++ toString ddiff.hour
                                    ++ " godzin "
                                    ++ toString ddiff.minute
                                    ++ " minut "
                                    ++ toString ddiff.second
                                    ++ " sekund"
                            , Html.br [] []
                            , Html.text <|
                                "Popełniłeś "
                                    ++ toString m.errorCount
                                    ++ (if m.errorCount <= 20 then
                                            case m.errorCount of
                                                1 ->
                                                    " błąd"

                                                2 ->
                                                    " błędy"

                                                3 ->
                                                    " błędy"

                                                4 ->
                                                    " błędy"

                                                _ ->
                                                    " błędów"
                                        else
                                            case m.errorCount % 10 of
                                                2 ->
                                                    " błędy"

                                                3 ->
                                                    " błędy"

                                                4 ->
                                                    " błędy"

                                                _ ->
                                                    " błędów"
                                       )
                            , Html.br [] []
                            , Html.br [] []
                            , Button.button
                                [ Button.primary, Button.onClick StartGame ]
                                [ Html.text "Zagraj jeszcze raz" ]
                            ]
                        ]

                    _ ->
                        [ Grid.col [ Col.attrs [ Html.style [ ( "text-align", "center" ) ] ] ]
                            [ Button.button
                                [ Button.primary, Button.onClick StartGame ]
                                [ Html.text "Zacznij grę" ]
                            ]
                        ]


viewLastStatus : Model -> Html Action
viewLastStatus m =
    Grid.row [ Grid.centerXs ] <|
        case m.currentMult of
            Just _ ->
                [ Grid.col [ Col.attrs [ Html.style [ ( "text-align", "center" ) ] ] ]
                    [ case m.lastCorrect of
                        Just True ->
                            Alert.success [ Html.text "Dobrze!" ]

                        Just False ->
                            Alert.danger [ Html.text "Źle!" ]

                        _ ->
                            Html.text ""
                    ]
                ]

            Nothing ->
                []
