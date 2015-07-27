import Color
import Html exposing (text)
import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Mouse
import Signal exposing (..)
import Window

relativeMouse : (Int, Int) -> (Int, Int) -> (Int, Int)
relativeMouse (ox, oy) (x, y) = (x - ox, -(y - oy))

center : (Int, Int) -> (Int, Int)
center (w, h) = (w // 2, h // 2)

render (x, y) =
  let forms = [ circle 15 |> filled Color.lightBlue
                          |> move (toFloat x, toFloat y) ]
  in color Color.gray <| collage 400 400 forms


main = map render <| relativeMouse (200, 200) <~ Mouse.position
