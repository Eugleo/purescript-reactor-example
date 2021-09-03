module Main where

import Prelude

import App.Canvas as Canvas
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Game.Color (blue400, gray200)
import Game.Config (Config)
import Game.Grid as Grid
import Game.KeyEvent (KeyEvent(..))
import Game.MouseEvent (MouseEvent(..))
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI (Canvas.component config) unit body

type State
  = { x :: Int, y :: Int, cursor :: Maybe { x :: Int, y :: Int } }

config :: Config State
config =
  { title: "Moving Dot"
  , init: { x: 0, y: 0, cursor: Nothing }
  , onMouse: Just onMouse
  , onKey: Just onKey
  , onTick: Nothing
  , draw: \{ x, y, cursor } ->
      let
        base = (Grid.set (Grid.empty 36 24) x y (Grid.Filled blue400))
      in
        case cursor of
          Nothing -> base
          Just c -> Grid.set base c.x c.y (Grid.Filled gray200)
  , stopWhen: const false
  }
  where
  onMouse s (MouseEvent { x, y }) = s { cursor = Just { x, y } }

  onKey { x, y, cursor } (KeyEvent "ArrowLeft" _) = { x: max (x - 1) 0, y, cursor }
  onKey { x, y, cursor } (KeyEvent "ArrowDown" _) = { x, y: min (y + 1) 23, cursor }
  onKey { x, y, cursor } (KeyEvent "ArrowUp" _) = { x, y: max (y - 1) 0, cursor }
  onKey { x, y, cursor } (KeyEvent "ArrowRight" _) = { x: min (x + 1) 35, y, cursor }
  onKey s _ = s
