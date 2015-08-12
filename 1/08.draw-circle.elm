import Color
import Html exposing (text)
import Graphics.Collage exposing (collage, filled, circle)
import Mouse
import Signal exposing ((<~), (~))
import Window

relativeMouse : (Int, Int) -> (Int, Int) -> (Int, Int)
relativeMouse (ox, oy) (x, y) = (x - ox, y - oy)

center : (Int, Int) -> (Int, Int)
center (w, h) = (w // 2, h // 2)

render (x, y) = collage 400 400 [filled Color.black <| circle 15]


main = Signal.map render <| relativeMouse
                              <~ (Signal.map center Window.dimensions)
                               ~ Mouse.position
