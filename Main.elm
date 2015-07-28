import Color
import Html exposing (text)
import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Mouse
import Random
import Signal exposing (..)
import Text
import Time exposing (..)
import Window


-- CONFIG
speed = 500
spawnInterval = 57 / speed
sizePill = 15
sizePlayer =sizePill


-- HELPER FUNCTIONS
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

tf : Float -> Float -> String -> Form
tf y scl str = Text.fromString str
                |> Text.color Color.gray
                |> Graphics.Collage.text
                |> scale scl
                |> move (0, y)


-- INPUT
delta = fps 30
input = (,) <~ map inSeconds delta
             ~ sampleOn delta (map2 relativeMouse
                                (map center Window.dimensions)
                                Mouse.position)

generator = Random.int 0 100
rand fn sig = let stamp = timestamp sig
                  seed = map (\(t, _) -> Random.initialSeed <| round t) stamp
                  rnd = map (Random.generate generator) seed
              in map (\(r, _) -> fn r) rnd

randX = rand (\r -> (width * (toFloat r / 100)) - hWidth)
randCol = rand (\r -> if r % 10 == 1 then Color.lightBlue else defaultPill.col)

interval = (every (second * spawnInterval))
event = mergeMany [ map Tick input
                  , map2 (\x col -> Add (newPill x col))
                         (randX interval)
                         (randCol interval)
                  , map (\_ -> Click) Mouse.clicks ]


-- MODEL
type alias Pill = {pos:Vec, vel:Vec, rad:Float, col:Color.Color}

defaultPill = { pos = (0, hHeight)
              , vel = (0, -speed)
              , rad = sizePill
              , col = Color.lightRed }

defaultPlayer = { defaultPill | pos <- (0, -hHeight - sizePlayer)
                              , rad <- sizePlayer
                              , col <- Color.black }

type State = Start | Play | Over
type alias Game = {player:Pill, pills:List Pill, score:Int, state:State}

defaultGame = { player = defaultPlayer
              , pills  = []
              , score  = 0
              , state  = Start }

newPill : Float -> Color.Color -> Pill
newPill x col = { defaultPill | pos <- (x, hHeight)
                              , col <- col }


-- UPDATE
type Event
  = Tick (Time, (Int, Int))
  | Add Pill
  | Click

stepPlay : Event -> Game -> Game
stepPlay event g =
  case event of
    Tick (t, mp) ->
      let hit pill = (vecLen <| vecSub g.player.pos pill.pos) <
                       g.player.rad + pill.rad
          unculled = List.filter (\{pos} -> snd pos > -hHeight) g.pills
          untouched = List.filter (not << hit) unculled
          touched = List.filter hit unculled
          hitColor c = not <| List.isEmpty
                           <| List.filter (\{col} -> col == c) touched
          hitBlue = hitColor Color.lightBlue
          hitRed = hitColor Color.lightRed
          out = let (x, y) = mp in abs (toFloat x) > hWidth ||
                                   abs (toFloat y) > hHeight
          g' = { g | player <- stepPlayer mp g.player
                 , pills  <- List.map (stepPill t) untouched
                 , score  <- if hitBlue then g.score + 1 else g.score }
      in if hitRed || out then { defaultGame | score <- g'.score
                                             , state <- Over }
         else g'
    Add p -> { g | pills <- p :: g.pills }
    _     -> g

click : Event -> Bool
click event =
  case event of
    Click -> True
    _     -> False

stepGame : Event -> Game -> Game
stepGame event ({state} as g) =
  let playGame = { defaultGame | state <- Play }
      toPlay = if click event then playGame else g
  in case state of
    Play  -> stepPlay event g
    _     -> toPlay

stepPlayer : (Int, Int) -> Pill -> Pill
stepPlayer (x, y) p = { p | pos <- (toFloat x, toFloat y) }

stepPill : Time -> Pill -> Pill
stepPill t p = { p | pos <- vecAdd p.pos (vecMulS p.vel t) }


-- DISPLAY
render : (Int, Int) -> Game -> Element
render (w, h) g =
  let fromPill {rad, col, pos} = circle rad |> filled col
                                            |> move pos
      txts = case g.state of
        Start -> [ tf  70 4 "Blue FiLL"
                 , tf   0 2 "Click to Start" ]
        Play  -> [ tf   0 4 (toString g.score) ]
        Over  -> [ tf  70 4 "Game Over"
                 , tf   0 4 (toString g.score)
                 , tf -50 2 "Click to Restart" ]

      forms = txts ++ (List.map fromPill <| g.player :: g.pills)
  in color Color.lightGray <| container w h middle
                           <| color Color.white
                           <| collage width height forms


-- MAIN
main = render <~ Window.dimensions ~ (foldp stepGame defaultGame event)
