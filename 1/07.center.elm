import Html exposing (text)
import Graphics.Element exposing (..)
import Mouse
import Window
import Signal exposing (..)


relativeMouse : (Int, Int) -> (Int, Int) -> (Int, Int)
relativeMouse (ox, oy) (x, y) = (x - ox, y - oy)

center : (Int, Int) -> (Int, Int)
center (w, h) = (w // 2, h // 2)

main = map show <| relativeMouse <~ (map center Window.dimensions)
                                  ~ Mouse.position
