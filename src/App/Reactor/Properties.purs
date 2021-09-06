module App.Reactor.Properties where

import Prelude
import Event.DefaultBehavior (DefaultBehavior)
import Event.KeypressEvent (KeypressEvent)
import Event.MouseEvent (MouseEvent)
import Event.TickEvent (TickEvent)
import Game.Action (Action)
import Graphics.Drawing (Drawing)

type Properties m world =
  { title :: String
  , width :: Int
  , height :: Int
  , cellSize :: Int
  , draw :: world -> Drawing
  , onTick :: TickEvent -> Action m world Unit
  , onKey :: KeypressEvent -> Action m world DefaultBehavior
  , onMouse :: MouseEvent -> Action m world DefaultBehavior
  }