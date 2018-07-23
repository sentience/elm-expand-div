module Examples exposing (Msg, update, view, subscriptions, init)

import Html exposing (..)
import Html.Attributes exposing (..)
import CssTransitions
import AnimationFrames


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { cssTransitions : CssTransitions.Model
    , animationFrames : AnimationFrames.Model
    }


type Msg
    = CssTransitionsMsg CssTransitions.Msg
    | AnimationFramesMsg AnimationFrames.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CssTransitionsMsg cssTransitionsMsg ->
            let
                ( cssTransitionsModel, cssTransitionsCmd ) =
                    CssTransitions.update cssTransitionsMsg model.cssTransitions
            in
                ( { model | cssTransitions = cssTransitionsModel }, Cmd.map CssTransitionsMsg cssTransitionsCmd )

        AnimationFramesMsg animationFramesMsg ->
            let
                ( animationFramesModel, animationFramesCmd ) =
                    AnimationFrames.update animationFramesMsg model.animationFrames
            in
                ( { model | animationFrames = animationFramesModel }, Cmd.map AnimationFramesMsg animationFramesCmd )


view : Model -> Html Msg
view model =
    let
        boxStyle =
            style
                [ ( "border", "1px solid #000" )
                , ( "font", "normal 18px/2 sans-serif" )
                , ( "margin", "0 auto 1em" )
                , ( "max-width", "960px" )
                , ( "text-align", "center" )
                ]

        itemViews =
            List.map viewItem items

        previewCount =
            3
    in
        div []
            [ div [ boxStyle ]
                [ CssTransitions.view
                    { previewCount = previewCount
                    , msgTag = CssTransitionsMsg
                    }
                    model.cssTransitions
                    itemViews
                ]
            , div [ boxStyle ]
                [ AnimationFrames.view
                    { previewCount = previewCount
                    , msgTag = AnimationFramesMsg
                    }
                    model.animationFrames
                    itemViews
                ]
            ]


viewItem : String -> Html Msg
viewItem item =
    div
        [ style
            [ ( "border", "1px solid #000" )
            ]
        ]
        [ text item ]


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
    Sub.batch
        [ CssTransitions.subscriptions model.cssTransitions
            |> Sub.map CssTransitionsMsg
        , AnimationFrames.subscriptions model.animationFrames
            |> Sub.map AnimationFramesMsg
        ]


init : ( Model, Cmd Msg )
init =
    let
        ( cssTransitionsModel, cssTransitionsMsg ) =
            CssTransitions.init

        ( animationFramesModel, animationFramesMsg ) =
            AnimationFrames.init
    in
        ( Model
            cssTransitionsModel
            animationFramesModel
        , Cmd.batch
            [ Cmd.map CssTransitionsMsg cssTransitionsMsg
            , Cmd.map AnimationFramesMsg animationFramesMsg
            ]
        )
