module Reactor where

import Data.Unit (Unit)
import Event.DefaultBehavior (DefaultBehavior)
import Event.KeypressEvent (KeypressEvent)
import Event.MouseEvent (MouseEvent)
import Event.TickEvent (TickEvent)
import Reactor.Action (Action)
import Graphics.Drawing (Drawing)

type Reactor m world =
  { title :: String
  , init :: world
  , draw :: world -> Drawing
  , onTick :: TickEvent -> Action m world Unit
  , onKey :: KeypressEvent -> Action m world DefaultBehavior
  , onMouse :: MouseEvent -> Action m world DefaultBehavior
  , width :: Int
  , height :: Int
  }
