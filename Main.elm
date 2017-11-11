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
    = CollapsingFrom Int
    | Collapsed
    | ExpandingTo Int
    | Expanded


type Msg
    = Toggle Int -- content length in px
    | ExpandEnd
    | CollapseStart


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( case ( model, msg ) of
        ( CollapsingFrom _, CollapseStart ) ->
            Collapsed

        ( CollapsingFrom _, Toggle _ ) ->
            Expanded

        ( Collapsed, Toggle length ) ->
            ExpandingTo length

        ( ExpandingTo _, ExpandEnd ) ->
            Expanded

        ( ExpandingTo _, Toggle length ) ->
            CollapsingFrom length

        ( Expanded, Toggle length ) ->
            CollapsingFrom length

        _ ->
            model
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
                        (Json.succeed ExpandEnd)
                    , style
                        [ ( "overflow", "hidden" )
                        , ( "transition", "height 0.5s" )
                        , ( "height"
                          , case model of
                                CollapsingFrom length ->
                                    (toString length) ++ "px"

                                Collapsed ->
                                    "0"

                                ExpandingTo length ->
                                    (toString length) ++ "px"

                                Expanded ->
                                    "auto"
                          )
                        ]
                    ]
                    (List.drop previewCount itemViews)
                ]
            , case model of
                CollapsingFrom _ ->
                    viewToggleButton "expand"

                Collapsed ->
                    viewToggleButton "expand"

                ExpandingTo _ ->
                    viewToggleButton "collapse"

                Expanded ->
                    viewToggleButton "collapse"
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


viewToggleButton : String -> Html Msg
viewToggleButton label =
    button
        [ on "click" decodeToggleClick
        , style
            [ ( "display", "block" )
            , ( "font-size", "inherit" )
            , ( "border", "1px solid #000" )
            , ( "margin", "0" )
            , ( "width", "100%" )
            ]
        ]
        [ text label ]


decodeToggleClick : Json.Decoder Msg
decodeToggleClick =
    let
        domPath =
            [ "currentTarget"
            , "parentNode"
            , "firstChild"
            , "lastChild"
            , "scrollHeight"
            ]
    in
        Json.map Toggle
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
    case model of
        CollapsingFrom _ ->
            AnimationFrame.times (always CollapseStart)

        _ ->
            Sub.none


init : ( Model, Cmd Msg )
init =
    ( Collapsed, Cmd.none )
