import Html exposing (text)
import Graphics.Element exposing (show)
import Mouse
import Window
import Signal exposing ((<~), (~))


relativeMouse : (Int, Int) -> (Int, Int) -> (Int, Int)
relativeMouse (ox, oy) (x, y) = (x - ox, y - oy)

center : (Int, Int) -> (Int, Int)
center (w, h) = (w // 2, h // 2)

main = Signal.map show <| relativeMouse
                            <~ (Signal.map center Window.dimensions)
                             ~ Mouse.position
