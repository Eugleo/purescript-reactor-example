module Main where

import Prelude

import App.ReactorPage as ReactorPage
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Event.KeypressEvent (KeypressEvent(..))
import Event.MouseEvent (MouseEvent(..))
import Event.TickEvent (TickEvent(..))
import Reactor.Action
  ( executeDefaultBehavior
  , get
  , modify_
  , preventDefaultBehavior
  , triggerPause
  , utilities
  )
import Reactor (Reactor)
import Graphics.Color (blue400, gray200)
import Graphics.CoordinateSystem (canvas, grid, wrt)
import Graphics.Drawing (fill)
import Graphics.Shape (cell)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Utilities (withJust)

main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI (ReactorPage.component world) unit body

type World =
  { x :: Number
  , y :: Number
  , velocity :: { x :: Number, y :: Number }
  , cursor :: Maybe { x :: Int, y :: Int }
  , paused :: Boolean
  }

world :: forall m. Reactor m World
world =
  { title: "Moving Dot", width: 20, height: 20, init, onMouse, onKey, onTick, draw }
  where
  init = { x: 0.0, y: 0.0, velocity: { x: 0.0, y: 0.0 }, cursor: Nothing, paused: false }

  draw s@{ cursor } = do
    fill blue400 $ cell $ { x: s.x, y: s.y } `wrt` canvas
    withJust cursor
      $ fill gray200 <<< cell <<< (_ `wrt` grid)

  onMouse (MouseEvent { x, y }) = do
    modify_ \s -> s { cursor = Just { x, y } }
    preventDefaultBehavior

  onKey (KeypressEvent key _) = do
    { cellSize } <- utilities
    let
      perSec = ((toNumber cellSize) * _)
      speed = perSec 30.0
    case key of
      "ArrowLeft" -> do
        modify_ \s -> s { velocity = { x: -speed, y: 0.0 } }
        preventDefaultBehavior
      "ArrowRight" -> do
        modify_ \s -> s { velocity = { x: speed, y: 0.0 } }
        preventDefaultBehavior
      "ArrowDown" -> do
        modify_ \s -> s { velocity = { x: 0.0, y: speed } }
        preventDefaultBehavior
      "ArrowUp" -> do
        modify_ \s -> s { velocity = { x: 0.0, y: -speed } }
        preventDefaultBehavior
      " " -> do
        triggerPause
        preventDefaultBehavior
      _ -> executeDefaultBehavior

  onTick (TickEvent { delta }) = do
    { velocity, x, y } <- get
    { bound } <- utilities
    modify_ \s ->
      let
        { x, y } =
          bound $ { x: x + velocity.x * delta, y: y + velocity.y * delta } `wrt` canvas
      in
        s { x = x, y = y }
