import Color
import Graphics.Collage as C
import Graphics.Element exposing (color)
import Html exposing (text)
import Mouse
import Signal exposing ((<~))
import Window

relativeMouse : (Int, Int) -> (Int, Int) -> (Int, Int)
relativeMouse (ox, oy) (x, y) = (x - ox, -(y - oy))

center : (Int, Int) -> (Int, Int)
center (w, h) = (w // 2, h // 2)

render (x, y) =
  let forms = [ C.circle 15 |> C.filled Color.lightBlue
                            |> C.move (toFloat x, toFloat y) ]
  in color Color.gray <| C.collage 400 400 forms


main = Signal.map render <| relativeMouse (200, 200) <~ Mouse.position
