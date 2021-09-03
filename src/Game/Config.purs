module Game.Config where

import Data.Maybe (Maybe)
import Game.Grid (Grid)
import Game.KeyEvent (KeyEvent)
import Game.MouseEvent (MouseEvent)

type Config s =
  { title :: String
  , init :: s
  , draw :: s -> Grid
  , onTick :: Maybe (s -> s)
  , onKey :: Maybe (s -> KeyEvent -> s)
  , onMouse :: Maybe (s -> MouseEvent -> s)
  }
