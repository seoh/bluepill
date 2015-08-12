import Html exposing (text)
import Graphics.Element exposing (show)
import Mouse

main = Signal.map show Mouse.position
