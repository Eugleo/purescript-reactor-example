module Game.Config where

import Data.Maybe (Maybe)
import Game.Grid (Grid)
import Game.Key (Key)

type Config s
  = { title :: String
    , init :: s
    , draw :: s -> Grid
    , onTick :: Maybe (s -> s)
    , onKey :: Maybe (s -> Key -> s)
    , onMouse :: Maybe (s -> Key -> s)
    , stopWhen :: s -> Boolean
    }
