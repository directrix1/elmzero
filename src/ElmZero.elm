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
    , maxVelocity : Float
    , acceleration : Float
    , mapTexture : Maybe Texture
    , mapScale : Float
    , moves : { x : Float, y : Float }
    }

initModel : Model
initModel = { position = {x = 0.0, y = 0.0}
            , facing = 0.0
            , turnRate = pi / 6000
            , wWidth = 1920
            , wHeight = 1080
            , xVelocity = 0
            , yVelocity = 0
            , velocity = 0
            , maxVelocity = 0.04
            , acceleration = 0.00001
            , mapTexture = Nothing
            , mapScale = 500
            , moves = {x = 0, y = 0}
            }

type Msg = XMove Float | YMove Float | NoMove | WinSize (Int, Int) | Tick Time | TextureLoaded (Result Error Texture)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        YMove direction ->
            ( let m = model.moves
                in
                { model | moves = { m | y = direction } }
            , Cmd.none
            )
        XMove direction ->
            ( let m = model.moves
                in
                { model | moves = { m | x = direction } }
            , Cmd.none
            )
        NoMove ->
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
                    maxP = m.mapScale
                    minP = -maxP
                    maxV = m.maxVelocity
                    facing = m.facing + m.moves.x * m.turnRate * time
                    velocity = (min maxV (max -maxV (m.velocity + m.moves.y * m.acceleration * time)))
                    xVelocity = m.xVelocity + (sin facing) * velocity
                    yVelocity = m.yVelocity + (cos facing) * velocity
                in
                    { m | position = {x = (min maxP (max minP m.position.x + (xVelocity * time))), y = (min maxP (max minP m.position.y + (yVelocity * time)))}
                        , facing = facing
                        , velocity = velocity
                    }
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
                37 -> XMove 1
                38 -> YMove 1
                39 -> XMove -1
                40 -> YMove -1
                _ -> NoMove
            )
        , Keyboard.ups
            (
            \kc -> case kc of
                37 -> XMove 0
                38 -> YMove 0
                39 -> XMove 0
                40 -> YMove 0
                _ -> NoMove
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
            Vertex (vec3 -1 1 1) (vec2 0 1)

        topRight =
            Vertex (vec3 1 1 1) (vec2 1 1)

        bottomLeft =
            Vertex (vec3 -1 -1 1) (vec2 0 0)

        bottomRight =
            Vertex (vec3 1 -1 1) (vec2 1 0)
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
