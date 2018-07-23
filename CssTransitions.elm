module CssTransitions exposing (Model, Msg, ViewConfig, init, subscriptions, update, view)

import AnimationFrame
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json


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


type alias ViewConfig msg =
    { previewCount : Int
    , msgTag : Msg -> msg
    }


view : ViewConfig msg -> Model -> List (Html msg) -> Html msg
view config model itemViews =
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
            [ div [] (List.take config.previewCount itemViews)
            , div
                [ on "transitionend" <|
                    case model of
                        ExpandingTo _ ->
                            Json.succeed (config.msgTag ExpandEnd)

                        _ ->
                            Json.fail "Not an expand transition"
                , style
                    [ ( "overflow", "hidden" )
                    , ( "transition", "height 0.5s" )
                    , ( "height"
                      , case model of
                            CollapsingFrom length ->
                                toString length ++ "px"

                            Collapsed ->
                                "0"

                            ExpandingTo length ->
                                toString length ++ "px"

                            Expanded ->
                                "auto"
                      )
                    ]
                ]
                (List.drop config.previewCount itemViews)
            ]
        , viewToggleButton model
            |> Html.map config.msgTag
        ]


viewToggleButton : Model -> Html Msg
viewToggleButton model =
    button
        [ on "click"
            (Json.map Toggle
                (Json.at
                    [ "currentTarget"
                    , "parentNode"
                    , "firstChild"
                    , "lastChild"
                    , "scrollHeight"
                    ]
                    Json.int
                )
            )
        , style
            [ ( "display", "block" )
            , ( "font-size", "inherit" )
            , ( "border", "1px solid #000" )
            , ( "margin", "0" )
            , ( "width", "100%" )
            ]
        ]
        [ case model of
            CollapsingFrom _ ->
                text "expand"

            Collapsed ->
                text "expand"

            ExpandingTo _ ->
                text "collapse"

            Expanded ->
                text "collapse"
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
