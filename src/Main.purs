module Main where

import Prelude
import App.Canvas as Canvas
import Color (hsv)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Game.Config (Config)
import Game.Grid as Grid
import Game.Key (Key(..))
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI (Canvas.component config) unit body

type State
  = { x :: Int, y :: Int }

config :: Config State
config =
  { title: "Moving Dot"
  , init: { x: 0, y: 0 }
  , onMouse: Nothing
  , onKey: Just onKey
  , onTick: Nothing
  , draw:
      \{ x, y } ->
        Grid.set (Grid.empty 36 24) x y (Grid.Filled (hsv (toNumber 1) (toNumber 1) (toNumber 1)))
  , stopWhen: const false
  }
  where
  onKey { x, y } (Key "ArrowLeft" _) = { x: max (x - 1) 0, y }

  onKey { x, y } (Key "ArrowDown" _) = { x, y: min (y + 1) 23 }

  onKey { x, y } (Key "ArrowUp" _) = { x, y: max (y - 1) 0 }

  onKey { x, y } (Key "ArrowRight" _) = { x: min (x + 1) 35, y }

  onKey s _ = s
