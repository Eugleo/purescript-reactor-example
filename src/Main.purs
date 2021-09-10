module Main where

import Prelude

import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Reactor (CoordinateSystem, Reactor, canvas, cell, executeDefaultBehavior, fill, get, grid, modify_, preventDefaultBehavior, runReactor, togglePause, utilities, wrt)
import Reactor.Events (KeypressEvent(..), MouseEvent(..), TickEvent(..))
import Reactor.Graphics.Colors as Color
import Reactor.Graphics.CoordinateSystem (withCoords)
import Reactor.Internal.Helpers (withJust)

main :: Effect Unit
main = runReactor reactor { title: "Puzzle", width: 20, height: 20 }

type Point = CoordinateSystem { x :: Number, y :: Number }
type Vector = Point

type World =
  { player :: Point
  , cursor :: Maybe Point
  , velocity :: { x :: Number, y :: Number }
  , paused :: Boolean
  }

reactor :: forall m. Reactor m World
reactor = { init, onMouse, onKey, onTick, draw }
  where
  init =
    { player: { x: 0, y: 0 } `wrt` grid
    , velocity: { x: 0.0, y: 0.0 }
    , cursor: Nothing
    , paused: false
    }

  draw { cursor, player } = do
    fill Color.blue400 $ cell player
    withJust cursor
      $ fill Color.gray200 <<< cell

  onMouse (MouseEvent { x, y }) = do
    modify_ \w -> w { cursor = Just $ { x, y } `wrt` grid }
    preventDefaultBehavior

  onKey (KeypressEvent key _) = do
    { cellSize } <- utilities
    let
      perSec = ((toNumber cellSize) * _)
      speed = perSec 30.0
    case key of
      "ArrowLeft" -> do
        modify_ \w -> w { velocity = { x: -speed, y: 0.0 } }
        preventDefaultBehavior
      "ArrowRight" -> do
        modify_ \w -> w { velocity = { x: speed, y: 0.0 } }
        preventDefaultBehavior
      "ArrowDown" -> do
        modify_ \w -> w { velocity = { x: 0.0, y: speed } }
        preventDefaultBehavior
      "ArrowUp" -> do
        modify_ \w -> w { velocity = { x: 0.0, y: -speed } }
        preventDefaultBehavior
      " " -> do
        togglePause
        preventDefaultBehavior
      _ -> executeDefaultBehavior

  onTick (TickEvent { delta }) = do
    { bound } <- utilities
    modify_ \w@{ velocity: { x, y }, player } ->
      withCoords player \p ->
        w { player = bound $ { x: p.x + x * delta, y: p.y + y * delta } `wrt` canvas }