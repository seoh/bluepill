import Color
import Html exposing (text)
import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Mouse
import Signal exposing (..)
import Time exposing (..)
import Window

relativeMouse : (Int, Int) -> (Int, Int) -> (Int, Int)
relativeMouse (ox, oy) (x, y) = (x - ox, -(y - oy))

center : (Int, Int) -> (Int, Int)
center (w, h) = (w // 2, h // 2)

type alias Vec = (Float, Float)

vecAdd : Vec -> Vec -> Vec
vecAdd (ax, ay) (bx, by) = (ax + bx, ay + by)

vecMulS : Vec -> Time -> Vec
vecMulS (x, y) t = (x * t, y * t)

type alias Pill = {pos:Vec, vel:Vec, rad:Float, col:Color.Color}

defaultPill = { pos = (0, 0)
              , vel = (0, -100)
              , rad = 15
              , col = Color.lightRed }

stepPill : Time -> Pill -> Pill
stepPill t p = { p | pos <- vecAdd p.pos (vecMulS p.vel t) }

render : (Int, Int) -> Pill -> Element
render (w, h) pill =
  let fromPill {rad, col, pos} = circle rad |> filled col
                                            |> move pos
      forms = [fromPill pill]
  in color Color.lightGray <| container w h middle
                           <| color Color.white
                           <| collage 400 400 forms

sig = map inSeconds (fps 30)
main = render <~ Window.dimensions ~ foldp stepPill defaultPill sig
