import Html exposing (text)
import Graphics.Element exposing (..)
import Mouse
import Window
import Signal exposing (..) -- no need to write Signal.map from here.

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
main = map show
         -- (map2 relativeMouse Window.dimensions Mouse.position)
         (relativeMouse <~ Window.dimensions ~  Mouse.position)
