module App.Reactor.State where

import Prelude

import Data.Maybe (Maybe)
import Graphics.CanvasAction (Context2D)
import Graphics.Grid (Cell, Grid)
import Halogen.Hooks (HookM)
import Halogen.Subscription (Listener)

type State m world =
  { context :: Maybe Context2D
  , mouseButtonPressed :: Boolean
  , renderListener :: Maybe (Listener (HookM m Unit))
  , lastTick :: Number
  , lastGrid :: Maybe (Grid Cell)
  , world :: world
  }
