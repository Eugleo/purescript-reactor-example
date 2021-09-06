module Game.Config where

import Data.Unit (Unit)
import Event.KeypressEvent (KeypressEvent)
import Event.MouseEvent (MouseEvent)
import Event.TickEvent (TickEvent)
import Game.Action (Action)
import Game.DefaultBehavior (DefaultBehavior)
import Game.Grid (Drawing, Grid)

type Config m state =
  { title :: String
  , init :: state
  , draw :: state -> Drawing
  , onTick :: TickEvent -> Action m state Unit
  , onKey :: KeypressEvent -> Action m state DefaultBehavior
  , onMouse :: MouseEvent -> Action m state DefaultBehavior
  , width :: Int
  , height :: Int
  }
