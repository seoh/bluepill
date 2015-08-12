import Html exposing (text)
import Graphics.Element exposing (show)
import Mouse

-- show <function> : a -> Graphics.Element.Element
main = show Mouse.position
