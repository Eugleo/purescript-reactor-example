module App.Canvas where

import Prelude
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect.Class (class MonadEffect, liftEffect)
import Graphics.Canvas (CanvasElement, getCanvasElementById, getContext2D)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import Web.HTML (HTMLElement)
import Web.HTML.HTMLCanvasElement (fromHTMLElement, HTMLCanvasElement)

canvasId :: String
canvasId = "canvas"

component :: forall q i o m. MonadEffect m => H.Component q i o m
component =
  Hooks.component \_ _ -> Hooks.do
    state /\ stateId <- Hooks.useState { context: Nothing }
    Hooks.useLifecycleEffect do
      canvas <- liftEffect $ getCanvasElementById canvasId
      Hooks.modify_ stateId \s -> s { context = getContext2D <$> canvas }
      pure Nothing
    Hooks.pure
      $ HH.div
          [ HP.classes [ H.ClassName "bg-gray-100 rounded-xl flex justify-center items-center" ] ]
          [ HH.canvas [ HP.id_ canvasId, HP.width 1080, HP.height 1080 ] ]
