module AnimationFrames exposing (Model, Msg, ViewConfig, update, view, subscriptions, init)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Animation exposing (px)
import Animation.Messenger
import Json.Decode as Json


type alias Model =
    { state : State
    , style : Animation.Messenger.State Msg
    }


type State
    = Collapsed
    | Expanding
    | Expanded


type Msg
    = Toggle Float -- content length in px
    | ExpandEnd
    | Animate Animation.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Toggle length ->
            ( case model.state of
                Collapsed ->
                    { model
                        | state = Expanding
                        , style =
                            Animation.interrupt
                                [ Animation.to [ Animation.height (px length) ]
                                , Animation.Messenger.send ExpandEnd
                                , Animation.set [ Animation.exactly "height" "auto" ]
                                ]
                                model.style
                    }

                Expanding ->
                    { model
                        | state = Collapsed
                        , style =
                            Animation.interrupt
                                [ Animation.to [ Animation.height (px 0.0) ]
                                ]
                                model.style
                    }

                Expanded ->
                    { model
                        | state = Collapsed
                        , style =
                            Animation.interrupt
                                [ Animation.to [ Animation.height (px 0.0) ]
                                ]
                                (Animation.styleWith
                                    (Animation.spring spring)
                                    [ Animation.height (px length)
                                    ]
                                )
                    }
            , Cmd.none
            )

        ExpandEnd ->
            ( { model | state = Expanded }, Cmd.none )

        Animate animMsg ->
            let
                ( newStyle, cmds ) =
                    Animation.Messenger.update animMsg model.style
            in
                ( { model | style = newStyle }, cmds )


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
                (List.concat
                    [ Animation.render model.style
                    , [ style
                            [ ( "overflow", "hidden" )
                            ]
                      ]
                    ]
                )
                (List.drop config.previewCount itemViews)
            ]
        , viewToggleButton model |> Html.map config.msgTag
        ]


viewToggleButton : { a | state : State } -> Html Msg
viewToggleButton { state } =
    button
        [ on "click" <|
            Json.map Toggle <|
                Json.at
                    [ "currentTarget"
                    , "parentNode"
                    , "firstChild"
                    , "lastChild"
                    , "scrollHeight"
                    ]
                    Json.float
        , style
            [ ( "display", "block" )
            , ( "font-size", "inherit" )
            , ( "border", "1px solid #000" )
            , ( "margin", "0" )
            , ( "width", "100%" )
            ]
        ]
        [ case state of
            Collapsed ->
                text "expand"

            Expanding ->
                text "collapse"

            Expanded ->
                text "collapse"
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Animation.subscription Animate [ model.style ]


init : ( Model, Cmd Msg )
init =
    ( { state = Collapsed
      , style =
            Animation.styleWith
                (Animation.spring spring)
                [ Animation.height (px 0.0)
                ]
      }
    , Cmd.none
    )


spring : { damping : Float, stiffness : Float }
spring =
    { stiffness = 200
    , damping = 23
    }
