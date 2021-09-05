module Game.Action where

import Prelude

import Control.Monad.Free (Free, foldFree, liftF)
import Control.Monad.Rec.Class (class MonadRec, Step(..), tailRecM)
import Control.Monad.Trans.Class (lift)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Random (randomInt)
import Game.DefaultBehavior (DefaultBehavior(..))
import Halogen.Hooks (HookM, StateId)
import Halogen.Hooks as Hooks

data ActionF m state a
  = RandomNumber Int Int (Int -> a)
  | Lift (m a)
  | Modify (state -> state) (state -> a)

derive instance functorActionF :: Functor m => Functor (ActionF m state)

newtype Action m state a = Action (Free (ActionF m state) a)

derive newtype instance functorAction :: Functor (Action m state)
derive newtype instance applyAction :: Apply (Action m state)
derive newtype instance applicativeAction :: Applicative (Action m state)
derive newtype instance bindAction :: Bind (Action m state)
derive newtype instance monadAction :: Monad (Action m state)
derive newtype instance semigroupAction :: Semigroup a => Semigroup (Action m state a)
derive newtype instance monoidAction :: Monoid a => Monoid (Action m state a)

instance monadEffectAction :: MonadEffect m => MonadEffect (Action m state) where
  liftEffect = Action <<< liftF <<< Lift <<< liftEffect

instance monadRecAction :: MonadRec (Action m state) where
  tailRecM k a = k a >>= case _ of
    Loop x -> tailRecM k x
    Done y -> pure y

randomPositive :: forall state m. Int -> Action m state Int
randomPositive = randomInRange 0

randomInRange :: forall state m. Int -> Int -> Action m state Int
randomInRange min max = Action $ liftF $ RandomNumber min max identity

modify :: forall state m. (state -> state) -> Action m state state
modify f = Action $ liftF $ Modify f identity

modify_ :: forall state m. (state -> state) -> Action m state Unit
modify_ = map (const unit) <<< modify

get :: forall state m. Action m state state
get = modify identity

pause :: forall state m. Action m { paused :: Boolean | state } Unit
pause = Action $ liftF $ Modify (\s -> s { paused = true }) (const unit)

unpause :: forall state m. Action m { paused :: Boolean | state } Unit
unpause = Action $ liftF $ Modify (\s -> s { paused = false }) (const unit)

triggerPause :: forall state m. Action m { paused :: Boolean | state } Unit
triggerPause = Action $ liftF $ Modify (\s -> s { paused = not s.paused }) (const unit)

preventDefaultBehavior :: forall state m. Action m { paused :: Boolean | state } DefaultBehavior
preventDefaultBehavior = Action $ liftF $ Modify identity (const Prevent)

performDefaultBehavior :: forall state m. Action m { paused :: Boolean | state } DefaultBehavior
performDefaultBehavior = Action $ liftF $ Modify identity (const Perform)

evalAction :: forall state m a. MonadEffect m => StateId state -> Action m state a -> HookM m a
evalAction stateId (Action action) = foldFree (go stateId) action
  where
  go :: forall state m a. MonadEffect m => StateId state -> ActionF m state a -> HookM m a
  go _ (RandomNumber min max cc) = do
    n <- liftEffect (randomInt min max)
    pure $ cc n
  go stateId (Modify modifyState cc) = do
    newState <- Hooks.modify stateId modifyState
    pure $ cc newState
  go _ (Lift ma) = lift ma
