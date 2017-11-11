module Main exposing (Msg, update, view, subscriptions, init)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import AnimationFrame
import Json.Decode as Json


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type Model
    = Expand Transition
    | Collapse Transition


type Transition
    = Starting Int Int
    | MovingTo Int
    | Done


type Msg
    = Transition Int Int
    | TransitionReady
    | TransitionEnd


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( case model of
        Expand transition ->
            case msg of
                Transition from to ->
                    Collapse (Starting from to)

                TransitionReady ->
                    case transition of
                        Starting _ to ->
                            Expand (MovingTo to)

                        MovingTo _ ->
                            model

                        Done ->
                            model

                TransitionEnd ->
                    Expand Done

        Collapse transition ->
            case msg of
                Transition from to ->
                    Expand (Starting from to)

                TransitionReady ->
                    case transition of
                        Starting _ to ->
                            Collapse (MovingTo to)

                        MovingTo _ ->
                            model

                        Done ->
                            model

                TransitionEnd ->
                    Collapse Done
    , Cmd.none
    )


view : Model -> Html Msg
view model =
    let
        itemViews =
            List.map viewItem items

        previewCount =
            3
    in
        div
            [ style
                [ ( "border", "1px solid #000" )
                , ( "font", "normal 18px/2 sans-serif" )
                , ( "margin", "0 auto" )
                , ( "max-width", "960px" )
                , ( "text-align", "center" )
                ]
            ]
            [ div []
                [ div [] (List.take previewCount itemViews)
                , div
                    [ on "transitionend"
                        (Json.succeed TransitionEnd)
                    , style
                        [ ( "overflow", "hidden" )
                        , ( "transition", "height 0.5s" )
                        , ( "height"
                          , case model of
                                Expand (Starting from _) ->
                                    (toString from) ++ "px"

                                Collapse (Starting from _) ->
                                    (toString from) ++ "px"

                                Expand (MovingTo to) ->
                                    (toString to) ++ "px"

                                Collapse (MovingTo to) ->
                                    (toString to) ++ "px"

                                Expand Done ->
                                    "auto"

                                Collapse Done ->
                                    "0"
                          )
                        ]
                    ]
                    (List.drop previewCount itemViews)
                ]
            , case model of
                Collapse _ ->
                    viewExpandButton

                Expand _ ->
                    viewCollapseButton
            ]


viewItem : String -> Html msg
viewItem =
    div
        [ style
            [ ( "border", "1px solid #000" )
            ]
        ]
        << List.singleton
        << text


viewExpandButton : Html Msg
viewExpandButton =
    button
        [ on "click" decodeExpand
        , style
            [ ( "display", "block" )
            , ( "font-size", "inherit" )
            , ( "border", "1px solid #000" )
            , ( "margin", "0" )
            , ( "width", "100%" )
            ]
        ]
        [ text "expand" ]


viewCollapseButton : Html Msg
viewCollapseButton =
    button
        [ on "click" decodeCollapse
        , style
            [ ( "display", "block" )
            , ( "font-size", "inherit" )
            , ( "border", "1px solid #000" )
            , ( "margin", "0" )
            , ( "width", "100%" )
            ]
        ]
        [ text "collapse" ]


decodeExpand : Json.Decoder Msg
decodeExpand =
    let
        domPath =
            [ "currentTarget"
            , "parentNode"
            , "firstChild"
            , "lastChild"
            , "scrollHeight"
            ]
    in
        Json.map (Transition 0)
            (Json.at domPath Json.int)


decodeCollapse : Json.Decoder Msg
decodeCollapse =
    let
        domPath =
            [ "currentTarget"
            , "parentNode"
            , "firstChild"
            , "lastChild"
            , "scrollHeight"
            ]
    in
        Json.map (\from -> Transition from 0)
            (Json.at domPath Json.int)


items : List String
items =
    [ "alpha"
    , "beta"
    , "charlie"
    , "delta"
    , "echo"
    , "foxtrot"
    , "golf"
    , "hotel"
    , "india"
    , "juliet"
    , "kilo"
    , "lima"
    , "mike"
    ]


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        transition =
            case model of
                Expand transition ->
                    transition

                Collapse transition ->
                    transition
    in
        case transition of
            Starting _ _ ->
                AnimationFrame.times (always TransitionReady)

            _ ->
                Sub.none


init : ( Model, Cmd Msg )
init =
    ( Collapse Done, Cmd.none )
