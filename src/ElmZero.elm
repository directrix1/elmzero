module ElmZero exposing (..)

{-| ElmZero - A racing game I made to learn elm.

# The main run loop
@docs main

-}

import Html exposing (h1, text)
import Html.Attributes exposing (id)

{-| The main run loop

# The main run loop runs the game
@docs main

-}
main : Html.Html a
main =
  h1 [id "hw"] [text "Hello World!"]
