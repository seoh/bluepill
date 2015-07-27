import Html exposing (text)
import Graphics.Element exposing (..)
import Mouse

main = Signal.map show Mouse.position
