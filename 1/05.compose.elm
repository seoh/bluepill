import Html exposing (text)
import Graphics.Element exposing (..)
import Mouse

relativeMouse : (Int, Int) -> (Int, Int)
relativeMouse (x,y) = (x-100, y-100)

{- 
  about Function Composition
  http://elm-lang.org/blog/announce/0.13#new-function-composition-operators

  > (<<)
  <function> : (a -> b) -> (c -> a) -> c -> b
  > (>>)
  <function> : (a -> b) -> (b -> c) -> a -> c
-}
myText : (Int, Int) -> Element
myText = show << relativeMouse

main = Signal.map myText Mouse.position
