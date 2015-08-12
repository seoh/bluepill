import Color
import Html exposing (text)
import Graphics.Element exposing (color)
import Graphics.Collage as C
import Mouse
import Signal exposing ((<~))
import Window

relativeMouse : (Int, Int) -> (Int, Int) -> (Int, Int)
relativeMouse (ox, oy) (x, y) = (x - ox, -(y - oy)) -- invert

center : (Int, Int) -> (Int, Int)
center (w, h) = (w // 2, h // 2)

render (x, y) = color Color.gray
                  <| C.collage 400 400 [C.move (toFloat x, toFloat y)
                                           <| C.filled Color.black
                                           <| C.circle 15]


main = Signal.map render <| relativeMouse (200, 200) <~ Mouse.position
