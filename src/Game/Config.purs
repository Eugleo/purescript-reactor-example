module Game.Config where

import Data.Unit (Unit)
import Event.KeypressEvent (KeypressEvent)
import Event.MouseEvent (MouseEvent)
import Event.TickEvent (TickEvent)
import Game.Action (Action)
import Game.DefaultBehavior (DefaultBehavior)
import Game.Grid (Grid)

type Config m state =
  { title :: String
  , init :: state
  , draw :: state -> Grid
  , onTick :: TickEvent -> Action m state Unit
  , onKey :: KeypressEvent -> Action m state DefaultBehavior
  , onMouse :: MouseEvent -> Action m state DefaultBehavior
  }
