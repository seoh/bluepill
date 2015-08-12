import Html exposing (text)
import Graphics.Element exposing (show)
import Mouse

relativeMouse (x,y) = show (x-100, y-100)

main = Signal.map relativeMouse Mouse.position
