module Game.DefaultBehavior where

import Prelude

import Effect.Class (class MonadEffect, liftEffect)
import Halogen.Hooks (HookM)
import Web.Event.Event (Event, preventDefault)

data DefaultBehavior = Prevent | Execute

derive instance eqDefaultBehavior :: Eq DefaultBehavior

optionallyPreventDefault ::
  forall m. MonadEffect m => DefaultBehavior -> Event -> HookM m Unit
optionallyPreventDefault behavior =
  when (behavior == Prevent)
    <<< liftEffect
    <<< preventDefault