--module Main exposing (main, update, view)


module Main exposing (..)

import Browser
import Dict
import Element
import Element.Border
import Element.Font
import Element.Input
import Html


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }


type Msg
    = IncMatch String Match
    | DecMatch String Match
    | UpdateNewPlayer String
    | DeletePlayer String
    | AddPlayer
    | NewGame


type Match
    = MatchTwo
    | MatchThree
    | MatchFour
    | MatchFive
    | MatchSix


type alias MatchCounts =
    { matchTwoCount : Int
    , matchThreeCount : Int
    , matchFourCount : Int
    , matchFiveCount : Int
    , matchSixCount : Int
    }


type alias Model =
    { players : Dict.Dict String MatchCounts
    , newPlayername : Maybe String
    }


initMatchCounts : MatchCounts
initMatchCounts =
    { matchTwoCount = 0
    , matchThreeCount = 0
    , matchFourCount = 0
    , matchFiveCount = 0
    , matchSixCount = 0
    }


pointsTotal : Maybe MatchCounts -> Int
pointsTotal matchCounts =
    case matchCounts of
        Nothing ->
            0

        Just counts ->
            (counts.matchTwoCount * 4)
                + (counts.matchThreeCount * 9)
                + (counts.matchFourCount * 16)
                + (counts.matchFiveCount * 25)
                + (counts.matchSixCount * 36)


init : Model
init =
    { players = Dict.empty
    , newPlayername = Nothing
    }


view : Model -> Html.Html Msg
view model =
    Element.layout [] (body model)


body : Model -> Element.Element Msg
body model =
    Element.column [ Element.width Element.fill, Element.height Element.fill ]
        [ header, center model ]


header : Element.Element msg
header =
    Element.row
        [ Element.Border.width 1
        , Element.paddingXY 20 10
        , Element.width Element.fill
        ]
        [ Element.downloadAs []
            { url = "index.html"
            , filename = "bold-counter.html"
            , label =
                Element.image
                    [ Element.height (Element.px 100) ]
                    { src = "Bold.png", description = "Logo" }
            }
        ]


countForm decMsg count incMsg =
    Element.row [ Element.spacingXY 10 0 ]
        [ Element.Input.button
            [ Element.Border.rounded 4
            , Element.Border.width 1
            , Element.Border.color (Element.rgb 0 0 0.4)
            , Element.alignBottom
            , Element.alignBottom
            , Element.width (Element.px 25)
            ]
            { label = Element.el [ Element.centerX ] (Element.text "-"), onPress = Just decMsg }
        , Element.text (String.fromInt count)
        , Element.Input.button
            [ Element.Border.rounded 4
            , Element.Border.width 1
            , Element.Border.color (Element.rgb 0 0 0.4)
            , Element.alignBottom
            , Element.width (Element.px 25)
            ]
            { label = Element.el [ Element.centerX ] (Element.text "+"), onPress = Just incMsg }
        ]


removePlayerButton : String -> Element.Element Msg
removePlayerButton playername =
    Element.el [ Element.Font.size 18, Element.width (Element.px 15), Element.alignRight ]
        (Element.Input.button
            [ Element.alignTop, Element.alignRight ]
            { label = Element.text "x", onPress = Just (DeletePlayer playername) }
        )


playernameCell playername =
    Element.row []
        [ Element.el
            [ Element.onRight (removePlayerButton playername) ]
            (Element.text playername)
        ]


center : Model -> Element.Element Msg
center model =
    let
        playernames =
            Dict.keys model.players
    in
    Element.column [ Element.spacingXY 50 50, Element.Font.size 35 ]
        [ Element.table [ Element.spacingXY 50 50 ]
            { data = playernames
            , columns =
                [ { header = Element.none
                  , width = Element.fill
                  , view = \playername -> playernameCell playername
                  }
                , { header = Element.el [ Element.Font.center ] (Element.text "Twos")
                  , width = Element.fill
                  , view =
                        \playername ->
                            countForm
                                (DecMatch playername MatchTwo)
                                (Maybe.withDefault initMatchCounts <| Dict.get playername model.players).matchTwoCount
                                (IncMatch playername MatchTwo)
                  }
                , { header = Element.el [ Element.Font.center ] (Element.text "Threes")
                  , width = Element.fill
                  , view =
                        \playername ->
                            countForm
                                (DecMatch playername MatchThree)
                                (Maybe.withDefault initMatchCounts <| Dict.get playername model.players).matchThreeCount
                                (IncMatch playername MatchThree)
                  }
                , { header = Element.el [ Element.Font.center ] (Element.text "Fours")
                  , width = Element.fill
                  , view =
                        \playername ->
                            countForm
                                (DecMatch playername MatchFour)
                                (Maybe.withDefault initMatchCounts <| Dict.get playername model.players).matchFourCount
                                (IncMatch playername MatchFour)
                  }
                , { header = Element.el [ Element.Font.center ] (Element.text "Fives")
                  , width = Element.fill
                  , view =
                        \playername ->
                            countForm
                                (DecMatch playername MatchFive)
                                (Maybe.withDefault initMatchCounts <| Dict.get playername model.players).matchFiveCount
                                (IncMatch playername MatchFive)
                  }
                , { header = Element.el [ Element.Font.center ] (Element.text "Sixs")
                  , width = Element.fill
                  , view =
                        \playername ->
                            countForm
                                (DecMatch playername MatchSix)
                                (Maybe.withDefault initMatchCounts <| Dict.get playername model.players).matchSixCount
                                (IncMatch playername MatchSix)
                  }
                , { header = Element.el [ Element.Font.center ] (Element.text "Total")
                  , width = Element.fill
                  , view =
                        \playername -> Element.text (String.fromInt (pointsTotal (Dict.get playername model.players)))
                  }
                ]
            }
        , Element.row [ Element.spacing 5 ]
            [ Element.Input.text
                [ Element.alignBottom
                ]
                { onChange = UpdateNewPlayer
                , text = Maybe.withDefault "" model.newPlayername
                , placeholder = Nothing
                , label = Element.Input.labelLeft [] Element.none
                }
            , Element.Input.button
                [ Element.Border.rounded 4
                , Element.Border.width 1
                , Element.Border.color (Element.rgb 0 0 0.4)
                , Element.alignBottom
                ]
                { label = Element.text "Add player", onPress = Just AddPlayer }
            ]
        , Element.row [ Element.alignRight ]
            [ Element.Input.button
                [ Element.Border.rounded 4
                , Element.Border.width 1
                , Element.Border.color (Element.rgb 0 0 0.4)
                , Element.alignRight
                ]
                { label = Element.text "New game", onPress = Just NewGame }
            ]
        ]


safeDec : Int -> Int
safeDec int =
    if int == 0 then
        0

    else
        int - 1


update : Msg -> Model -> Model
update msg model =
    let
        _ =
            Debug.log "model" model

        _ =
            Debug.log "msg" msg
    in
    case msg of
        IncMatch playername match ->
            let
                updateFn =
                    case match of
                        MatchTwo ->
                            \maybeMatchCounts ->
                                case maybeMatchCounts of
                                    Nothing ->
                                        Nothing

                                    Just counts ->
                                        Just { counts | matchTwoCount = counts.matchTwoCount + 1 }

                        MatchThree ->
                            \maybeMatchCounts ->
                                case maybeMatchCounts of
                                    Nothing ->
                                        Nothing

                                    Just counts ->
                                        Just { counts | matchThreeCount = counts.matchThreeCount + 1 }

                        MatchFour ->
                            \maybeMatchCounts ->
                                case maybeMatchCounts of
                                    Nothing ->
                                        Nothing

                                    Just counts ->
                                        Just { counts | matchFourCount = counts.matchFourCount + 1 }

                        MatchFive ->
                            \maybeMatchCounts ->
                                case maybeMatchCounts of
                                    Nothing ->
                                        Nothing

                                    Just counts ->
                                        Just { counts | matchFiveCount = counts.matchFiveCount + 1 }

                        MatchSix ->
                            \maybeMatchCounts ->
                                case maybeMatchCounts of
                                    Nothing ->
                                        Nothing

                                    Just counts ->
                                        Just { counts | matchSixCount = counts.matchSixCount + 1 }
            in
            { model | players = Dict.update playername updateFn model.players }

        DecMatch playername match ->
            let
                updateFn =
                    case match of
                        MatchTwo ->
                            \maybeMatchCounts ->
                                case maybeMatchCounts of
                                    Nothing ->
                                        Nothing

                                    Just counts ->
                                        Just { counts | matchTwoCount = safeDec counts.matchTwoCount }

                        MatchThree ->
                            \maybeMatchCounts ->
                                case maybeMatchCounts of
                                    Nothing ->
                                        Nothing

                                    Just counts ->
                                        Just { counts | matchThreeCount = safeDec counts.matchThreeCount }

                        MatchFour ->
                            \maybeMatchCounts ->
                                case maybeMatchCounts of
                                    Nothing ->
                                        Nothing

                                    Just counts ->
                                        Just { counts | matchFourCount = safeDec counts.matchFourCount }

                        MatchFive ->
                            \maybeMatchCounts ->
                                case maybeMatchCounts of
                                    Nothing ->
                                        Nothing

                                    Just counts ->
                                        Just { counts | matchFiveCount = safeDec counts.matchFiveCount }

                        MatchSix ->
                            \maybeMatchCounts ->
                                case maybeMatchCounts of
                                    Nothing ->
                                        Nothing

                                    Just counts ->
                                        Just { counts | matchSixCount = safeDec counts.matchSixCount }
            in
            { model | players = Dict.update playername updateFn model.players }

        UpdateNewPlayer updatedNewPlayername ->
            { model
                | newPlayername =
                    if updatedNewPlayername == "" then
                        Nothing

                    else
                        Just updatedNewPlayername
            }

        DeletePlayer playername ->
            let
                players =
                    Dict.remove playername model.players
            in
            { model | players = players }

        AddPlayer ->
            case model.newPlayername of
                Nothing ->
                    model

                Just newPlayername ->
                    let
                        existing =
                            Dict.get newPlayername model.players
                    in
                    case existing of
                        Nothing ->
                            let
                                players =
                                    Dict.insert newPlayername initMatchCounts model.players
                            in
                            { model | players = players, newPlayername = Nothing }

                        Just _ ->
                            { model | newPlayername = Nothing }

        NewGame ->
            let
                keys =
                    Dict.keys model.players
            in
            { model | players = resetDict keys model.players }


resetDict : List String -> Dict.Dict String MatchCounts -> Dict.Dict String MatchCounts
resetDict keys dict =
    case keys of
        [] ->
            dict

        key :: rest ->
            let
                d =
                    Dict.insert key initMatchCounts dict
            in
            resetDict rest d
