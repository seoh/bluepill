import Html exposing (text)
import Graphics.Element exposing (show)
import Mouse
import Window
import Signal exposing ((~), (<~))

relativeMouse : (Int, Int) -> (Int, Int) -> (Int, Int)
relativeMouse (ox, oy) (x, y) = (x - ox, y - oy)

{-
  > Signal.map2
  <function: map2>
      : (a -> b -> c)
        -> Signal.Signal a -> Signal.Signal b -> Signal.Signal c

then you can use operators for writing simple code.

  > (<~)
  <function> : (a -> b) -> Signal.Signal a -> Signal.Signal b
  > (~)
  <function> : Signal.Signal (a -> b) -> Signal.Signal a -> Signal.Signal b

  see also http://elm-lang.org/docs/syntax#mapping
-}
main = Signal.map show
        --  (Signal.map2 relativeMouse Window.dimensions Mouse.position)
         (relativeMouse <~ Window.dimensions ~  Mouse.position)
