import Color
import Html exposing (text)
import Graphics.Collage as C
import Graphics.Element as E exposing (Element)
import Mouse
import Signal exposing ((<~), (~))
import Time exposing (Time, inSeconds, fps)
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
              , vel = (0, -30)
              , rad = 15
              , col = Color.lightRed
              }

defaultPlayer = { defaultPill | col <- Color.black }

type alias Game = {player:Pill, pill:Pill}

defaultGame = { player = defaultPlayer
              , pill   = defaultPill
              }

stepGame : Time -> Game -> Game
stepGame t g= { g | pill <- stepPill t g.pill }

stepPill : Time -> Pill -> Pill
stepPill t p = { p | pos <- vecAdd p.pos (vecMulS p.vel t) }

render : (Int, Int) -> Game -> Element
render (w, h) game =
  let fromPill {rad, col, pos} = C.circle rad |> C.filled col
                                              |> C.move pos
      forms = [fromPill game.player, fromPill game.pill]
  in E.color Color.lightGray <| E.container w h E.middle
                             <| E.color Color.white
                             <| C.collage 400 400 forms

sig = Signal.map inSeconds (fps 30)
main = render <~ Window.dimensions ~ Signal.foldp stepGame defaultGame sig
