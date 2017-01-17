module ElmZero exposing (main)

{-| ElmZero - A racing game I made to learn elm.

# The main run loop
@docs main

-}

import Html exposing (..)
import Keyboard exposing (..)
import Platform.Cmd as Cmd
import Platform.Sub as Sub
import Svg exposing (..)
import Svg.Attributes exposing (..)

type alias Position = {x: Float, y: Float}
type alias Model =
    { position : Position
    , facing : Float
    , turnRate : Float
    }

initModel : Model
initModel = { position = {x = 0.0, y = 0.0}
            , facing = pi * 3 / 2
            , turnRate = pi / 30
            }

type Msg = UpPressed | DownPressed | LeftPressed | RightPressed | NothingPressed

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        UpPressed ->
            ( { model | position = {x = model.position.x + (cos model.facing), y = model.position.y + (sin model.facing)}}
            , Cmd.none
            )
        DownPressed ->
            ( { model | position = {x = model.position.x - (cos model.facing), y = model.position.y - (sin model.facing)}}
            , Cmd.none
            )
        LeftPressed ->
            ( { model | facing = model.facing + model.turnRate}
            , Cmd.none
            )
        RightPressed ->
            ( { model | facing = model.facing - model.turnRate}
            , Cmd.none
            )
        NothingPressed ->
            ( model
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Keyboard.downs
        (
        \kc -> case kc of
            37 -> RightPressed
            38 -> UpPressed
            39 -> LeftPressed
            40 -> DownPressed
            _ -> NothingPressed
        )

view : Model -> (Html Msg)
view model =
      svg
        [ version "1.1", x "0", y "0", width "100%", height "99%"
        ]
        [   g [rotate "1.0"]
            [   image [x (toString model.position.x), y (toString model.position.y), width "1024px", height "1024px", xlinkHref "../resources/lava.png"] []
            ]
        ]

{-| The main run loop -}
main : Program Never Model Msg
main =
    Html.program
    {   init = (initModel, Cmd.none)
    ,   subscriptions = subscriptions
    ,   update = update
    ,   view = view
    }
