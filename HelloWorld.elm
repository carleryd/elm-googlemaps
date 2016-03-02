module HelloWorld where
import Html exposing (text)
import Signal exposing (..)

port requestMarker : Signal String
port requestMarker =
  signalOfMarker

main =
  text "Hello, World!"
