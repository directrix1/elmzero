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
import Window as Window
import Task as Task

type alias Position = {x: Float, y: Float}
type alias Model =
    { position : Position
    , facing : Float
    , turnRate : Float
    , wWidth : Int
    , wHeight : Int
    }

initModel : Model
initModel = { position = {x = 512.0, y = 0.0}
            , facing = 3 * pi / 2
            , turnRate = pi / 30
            , wWidth = 1920
            , wHeight = 1080
            }

type Msg = UpPressed | DownPressed | LeftPressed | RightPressed | NothingPressed | WinSize (Int, Int)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        UpPressed ->
            ( { model | position = {x = model.position.x - (cos model.facing), y = model.position.y - (sin model.facing)}}
            , Cmd.none
            )
        DownPressed ->
            ( { model | position = {x = model.position.x + (cos model.facing), y = model.position.y + (sin model.facing)}}
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
        WinSize (w, h) ->
            ( { model | wWidth = w, wHeight = h}
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [   Keyboard.downs
                (
                \kc -> case kc of
                    37 -> RightPressed
                    38 -> UpPressed
                    39 -> LeftPressed
                    40 -> DownPressed
                    _ -> NothingPressed
                )
        ,   Window.resizes (\size -> (WinSize (size.width, size.height)))
        ]

winsizeToMsg : Window.Size -> Msg
winsizeToMsg size = WinSize (size.width, size.height)

view : Model -> (Html Msg)
view model =
      svg
        [ version "1.1", x "0", y "0", width (toString <| model.wWidth - 10), height (toString <| model.wHeight - 10)
        ]
        [   g [transform ("translate(" ++ (toString ((toFloat model.wWidth) / 2)) ++ " " ++ (toString model.wHeight) ++ ") rotate(" ++ (toString -((model.facing * 180 / pi) - 90)) ++ ") translate(" ++ (toString (-model.position.x)) ++ " " ++ (toString (-model.position.y)) ++ ")")]
            [   image [x "0", y "0", width "1024px", height "1024px", xlinkHref "../resources/lava.png"] []
            ]
        ]

{-| The main run loop -}
main : Program Never Model Msg
main =
    Html.program
    {   init = (initModel, Task.perform winsizeToMsg Window.size)
    ,   subscriptions = subscriptions
    ,   update = update
    ,   view = view
    }
