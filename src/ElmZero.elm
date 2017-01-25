module ElmZero exposing (main)

{-| ElmZero - A racing game I made to learn elm.

# The main run loop
@docs main

-}

import Html exposing (..)
import Html.Attributes exposing (width, height, style)
import Keyboard exposing (..)
import Platform.Cmd as Cmd
import Platform.Sub as Sub
import Window as Window
import Task as Task
import AnimationFrame exposing (diffs)
import Time exposing (Time)
import WebGL exposing (Mesh, Shader, Entity)
import WebGL.Texture as Texture exposing (Texture, Error)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)

type alias Position = {x: Float, y: Float}
type alias Model =
    { position : Position
    , facing : Float
    , turnRate : Float
    , wWidth : Int
    , wHeight : Int
    , xVelocity : Float
    , yVelocity : Float
    , velocity : Float
    , acceleration : Float
    , mapTexture : Maybe Texture
    , mapScale : Float
    }

initModel : Model
initModel = { position = {x = 0.0, y = 0.0}
            , facing = 0.0
            , turnRate = pi / 60
            , wWidth = 1920
            , wHeight = 1080
            , xVelocity = 0
            , yVelocity = 0
            , velocity = 0
            , acceleration = 0.0001
            , mapTexture = Nothing
            , mapScale = 100
            }

type Msg = UpPressed | DownPressed | LeftPressed | RightPressed | NothingPressed | WinSize (Int, Int) | Tick Time | TextureLoaded (Result Error Texture)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        UpPressed ->
            ( { model | velocity = model.velocity + model.acceleration}
            , Cmd.none
            )
        DownPressed ->
            ( { model | velocity = model.velocity - model.acceleration}
            , Cmd.none
            )
        LeftPressed ->
            ( { model | facing = model.facing - model.turnRate}
            , Cmd.none
            )
        RightPressed ->
            ( { model | facing = model.facing + model.turnRate}
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
        Tick time ->
            ( let
                    m = model
                    maxP = model.mapScale
                    minP = -maxP
                    xVelocity = m.xVelocity + (sin model.facing) * model.velocity
                    yVelocity = m.yVelocity + (cos model.facing) * model.velocity
                in
                    { m | position = {x = (min maxP (max minP m.position.x + (xVelocity * time))), y = (min maxP (max minP m.position.y + (yVelocity * time)))}}
            , Cmd.none
            )
        TextureLoaded texResult ->
            ( { model | mapTexture = Result.toMaybe texResult }, Cmd.none )

subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Keyboard.downs
            (
            \kc -> case kc of
                37 -> RightPressed
                38 -> UpPressed
                39 -> LeftPressed
                40 -> DownPressed
                _ -> NothingPressed
            )
        , Window.resizes (\size -> (WinSize (size.width, size.height)))
        , diffs Tick
        ]

winsizeToMsg : Window.Size -> Msg
winsizeToMsg size = WinSize (size.width, size.height)

type alias Vertex = { position : Vec3
                    , coord : Vec2
                    }

scene : Model -> Texture -> List Entity
scene model texture =
    let
        perspective = Mat4.mul
            (Mat4.makePerspective 30 (toFloat model.wWidth / toFloat model.wHeight) 0.01 100)
            (Mat4.identity
                |> Mat4.rotate (pi / 1.9) Vec3.i
                |> Mat4.rotate model.facing Vec3.k
                |> Mat4.translate (vec3 model.position.x model.position.y 0)
                |> Mat4.scale (vec3 model.mapScale model.mapScale 1))
    in
        [ WebGL.entity
            vertexShader
            fragmentShader
            squareMesh
            { texture = texture
            , perspective = perspective
            }
        ]

squareMesh : Mesh Vertex
squareMesh = square |> WebGL.triangles

square : List (Vertex, Vertex, Vertex)
square =
    let
        topLeft =
            Vertex (vec3 -1 1 0.5) (vec2 0 1)

        topRight =
            Vertex (vec3 1 1 0.5) (vec2 1 1)

        bottomLeft =
            Vertex (vec3 -1 -1 0.5) (vec2 0 0)

        bottomRight =
            Vertex (vec3 1 -1 0.5) (vec2 1 0)
    in
        [ ( topLeft, topRight, bottomLeft )
        , ( bottomLeft, topRight, bottomRight )
        ]

view : Model -> (Html Msg)
view model =
    WebGL.toHtmlWith
        [ WebGL.depth 1
        , WebGL.antialias
        ]
        [ width model.wWidth
        , height model.wHeight
        , style [ ("display", "block") ]
        ]
        (model.mapTexture
            |> Maybe.map (scene model)
            |> Maybe.withDefault []
        )

{-| The main run loop -}
main : Program Never Model Msg
main =
    Html.program
    { init =
        ( initModel
        , Cmd.batch
            [ Task.perform winsizeToMsg Window.size
            , Task.attempt TextureLoaded (Texture.load "../resources/lava.png")
            ]
        )
    , subscriptions = subscriptions
    , update = update
    , view = view
    }

-- Shaders


type alias Uniforms =
    { texture : Texture
    , perspective : Mat4
    }


vertexShader : Shader Vertex Uniforms { vcoord : Vec2 }
vertexShader =
    [glsl|
        attribute vec3 position;
        attribute vec2 coord;
        uniform mat4 perspective;
        varying vec2 vcoord;
        void main () {
          gl_Position = perspective * vec4(position, 1.0);
          vcoord = coord;
        }
    |]


fragmentShader : Shader {} Uniforms { vcoord : Vec2 }
fragmentShader =
    [glsl|
        precision mediump float;
        uniform sampler2D texture;
        varying vec2 vcoord;
        void main () {
          gl_FragColor = texture2D(texture, vcoord);
        }
    |]
