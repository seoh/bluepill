import Color
import Html exposing (text)
import Graphics.Collage as C
import Graphics.Element as E exposing (Element)
import Mouse
import Signal exposing ((<~), (~))
import Time exposing (Time, inSeconds, fps)
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

vecSub : Vec -> Vec -> Vec
vecSub (ax, ay) (bx, by) = (ax - bx, ay - by)

vecLen : Vec -> Float
vecLen (x, y) = sqrt (x * x + y * y)

vecMulS : Vec -> Time -> Vec
vecMulS (x, y) t = (x * t, y * t)

type alias Pill = {pos:Vec, vel:Vec, rad:Float, col:Color.Color}

defaultPill = { pos = (0, hHeight)
              , vel = (0, -30)
              , rad = 15
              , col = Color.lightRed }

defaultPlayer = { defaultPill | pos <- (0, 0)
                              , col <- Color.black }

type alias Game = {player:Pill, pill:Pill}

defaultGame = { player = defaultPlayer
              , pill   = defaultPill }

stepGame : (Time, (Int, Int)) -> Game -> Game
stepGame (t, mp) ({player, pill} as g) =
  let hit = (vecLen <| vecSub player.pos pill.pos) < player.rad + pill.rad
      player' = { player | col <- if hit then Color.lightRed else player.col }
  in { g | player <- stepPlayer mp player'
         , pill   <- stepPill t g.pill }

stepPlayer : (Int, Int) -> Pill -> Pill
stepPlayer (x, y) p = { p | pos <- (toFloat x, toFloat y) }

stepPill : Time -> Pill -> Pill
stepPill t p = { p | pos <- vecAdd p.pos (vecMulS p.vel t) }

render : (Int, Int) -> Game -> Element
render (w, h) game =
  let fromPill {rad, col, pos} = C.circle rad |> C.filled col
                                              |> C.move pos
      forms = [fromPill game.player, fromPill game.pill]
  in E.color Color.lightGray <| E.container w h E.middle
                             <| E.color Color.white
                             <| C.collage width height forms

delta = fps 30
input = (,) <~ Signal.map inSeconds delta
             ~ Signal.sampleOn delta
                (relativeMouse
                   <~ (Signal.map center Window.dimensions)
                    ~ Mouse.position)


main = render <~ Window.dimensions ~ Signal.foldp stepGame defaultGame input
