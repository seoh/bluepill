import Color
import Html exposing (text)
import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Mouse
import Signal exposing (..)
import Time exposing (..)
import Window

(width, height) = (400, 400)
(hWidth, hHeight) = (width/2, height/2)

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

defaultPill = { pos = (0, hHeight)
              , vel = (0, -30)
              , rad = 15
              , col = Color.lightRed }

defaultPlayer = { defaultPill | col <- Color.black }

type alias Game = {player:Pill, pill:Pill}

defaultGame = { player = defaultPlayer
              , pill   = defaultPill }

stepGame : (Time, (Int, Int)) -> Game -> Game
stepGame (t, mp) g = { g | player <- stepPlayer mp g.player
                         , pill   <- stepPill t g.pill }

stepPlayer : (Int, Int) -> Pill -> Pill
stepPlayer (x, y) p = { p | pos <- (toFloat x, toFloat y) }

stepPill : Time -> Pill -> Pill
stepPill t p = { p | pos <- vecAdd p.pos (vecMulS p.vel t) }

render : (Int, Int) -> Game -> Element
render (w, h) game =
  let fromPill {rad, col, pos} = circle rad |> filled col
                                            |> move pos
      forms = [fromPill game.player, fromPill game.pill]
  in color Color.lightGray <| container w h middle
                           <| color Color.white
                           <| collage width height forms

delta = fps 30
input = (,) <~ map inSeconds delta
             ~ sampleOn delta (map2 relativeMouse (map center Window.dimensions) Mouse.position)

main = render <~ Window.dimensions ~ foldp stepGame defaultGame input
