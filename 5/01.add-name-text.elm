import Color
import Html exposing (text)
import Graphics.Collage as C exposing (Form)
import Graphics.Element as E exposing (Element)
import Mouse
import Random
import Signal exposing ((<~), (~))
import Text exposing (fromString)
import Time exposing (Time, inSeconds, fps, every, second, timestamp)
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
              , vel = (0, -100)
              , rad = 15
              , col = Color.lightRed }

defaultPlayer = { defaultPill | pos <- (0, 0)
                              , col <- Color.black }

type alias Game = {player:Pill, pills:List Pill}

defaultGame = { player = defaultPlayer
              , pills  = [] }

newPill : Float -> Color.Color -> Pill
newPill x col = { defaultPill | pos <- (x, hHeight)
                              , col <- col }

type Event
  = Tick (Time, (Int, Int))
  | Add Pill

stepGame : Event -> Game -> Game
stepGame event ({player, pills} as g) =
  case event of
    Tick (t, mp) ->
      let hit pill = (vecLen <| vecSub player.pos pill.pos) < player.rad + pill.rad
          unculled = List.filter (\{pos} -> snd pos > -hHeight) pills
          untouched = List.filter (not << hit) unculled
      in { g | player <- stepPlayer mp player
             , pills  <- List.map (stepPill t) untouched }
    Add p ->
      { g | pills <- p :: g.pills }

stepPlayer : (Int, Int) -> Pill -> Pill
stepPlayer (x, y) p = { p | pos <- (toFloat x, toFloat y) }

stepPill : Time -> Pill -> Pill
stepPill t p = { p | pos <- vecAdd p.pos (vecMulS p.vel t) }


tf : Float -> Float -> String -> Form
tf y scl str = Text.fromString str
                |> Text.color Color.gray
                |> C.text
                |> C.scale scl
                |> C.move (0, y)

render : (Int, Int) -> Game -> Element
render (w, h) g =
  let fromPill {rad, col, pos} = C.circle rad |> C.filled col
                                              |> C.move pos
      txt = tf 0 2 "seoh"
      forms = txt :: (List.map fromPill <| g.player :: g.pills)
  in E.color Color.lightGray <| E.container w h E.middle
                             <| E.color Color.white
                             <| C.collage width height forms

delta = fps 30
input = (,) <~ Signal.map inSeconds delta
             ~ Signal.sampleOn delta
                 (relativeMouse
                    <~ (Signal.map center Window.dimensions)
                     ~ Mouse.position)

generator = Random.int 0 100
rand fn sig = let stamp = timestamp sig
                  seed  = Signal.map (\(t, _) -> Random.initialSeed <| round t) stamp
                  rnd   = Signal.map (Random.generate generator) seed
              in Signal.map (\(r, _) -> fn r) rnd

randX   = rand (\r -> (width * (toFloat r / 100)) - hWidth)
randCol = rand (\r -> if r % 10 == 1 then Color.lightBlue else defaultPill.col)

interval = (every (second * 2))
event = Signal.mergeMany [
         Signal.map Tick input,
         (\x col -> Add (newPill x col)) <~ (randX interval) ~ (randCol interval)]

main = render <~ Window.dimensions ~ (Signal.foldp stepGame defaultGame event)
