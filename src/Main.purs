module Main where

import Prelude

import App.Canvas as Canvas
import Data.Int (floor)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Event.KeypressEvent (KeypressEvent(..))
import Event.MouseEvent (MouseEvent(..))
import Event.TickEvent (TickEvent(..))
import Game.Action (get, modify_, pause, executeDefaultBehavior, preventDefaultBehavior, triggerPause)
import Game.Color (blue400, gray200)
import Game.Config (Config)
import Game.Grid as Grid
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI (Canvas.component config) unit body

type State
  =
  { x :: Number
  , y :: Number
  , velocity :: { x :: Number, y :: Number }
  , cursor :: Maybe { x :: Int, y :: Int }
  , paused :: Boolean
  }

config :: forall m. Config m State
config =
  { title: "Moving Dot"
  , init: { x: 0.0, y: 0.0, velocity: { x: 0.0, y: 0.0 }, cursor: Nothing, paused: false }
  , onMouse: onMouse
  , onKey: onKey
  , onTick: onTick
  , draw: \s@{ cursor } ->
      let
        { x, y } = snap s 30.0
        base = (Grid.set (Grid.empty 36 24) x y (Grid.Filled blue400))
      in
        case cursor of
          Nothing -> base
          Just c -> Grid.set base c.x c.y (Grid.Filled gray200)
  }
  where
  onMouse (MouseEvent { x, y }) = do
    modify_ \s -> s { cursor = Just { x, y } }
    preventDefaultBehavior

  onKey (KeypressEvent key _) = do
    case key of
      "ArrowLeft" -> do
        modify_ \s -> s { velocity = { x: cellsPerSec (-3.0), y: 0.0 } }
        preventDefaultBehavior
      "ArrowRight" -> do
        modify_ \s -> s { velocity = { x: cellsPerSec 3.0, y: 0.0 } }
        preventDefaultBehavior
      "ArrowDown" -> do
        modify_ \s -> s { velocity = { x: 0.0, y: cellsPerSec 3.0 } }
        preventDefaultBehavior
      "ArrowUp" -> do
        modify_ \s -> s { velocity = { x: 0.0, y: cellsPerSec (-3.0) } }
        preventDefaultBehavior
      " " -> do
        triggerPause
        preventDefaultBehavior
      _ -> executeDefaultBehavior

  onTick (TickEvent { delta }) = do
    { velocity, x, y } <- get
    modify_ \s -> s
      { x = bound (x + velocity.x * delta) 1080.0
      , y = bound (y + velocity.y * delta) 720.0
      }

  cellsPerSec = (_ * 30.0)
  snap { x, y } size = { x: floor (x / size), y: floor (y / size) }
  bound n b = max (min n b) 0.0
