module Game.Action where

import Prelude

import App.Reactor.State (State)
import Control.Monad.Free (Free, foldFree, liftF)
import Control.Monad.Rec.Class (class MonadRec, Step(..), tailRecM)
import Control.Monad.Trans.Class (lift)
import Data.Int (toNumber)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Random (randomInt)
import Event.DefaultBehavior (DefaultBehavior(..))
import Graphics.CoordinateSystem (CoordinateSystem(..))
import Graphics.Shape (Point)
import Halogen.Hooks (HookM, StateId)
import Halogen.Hooks as Hooks
import Type.Proxy (Proxy(..))

type Utilities =
  { width :: Int
  , height :: Int
  , cellSize :: Int
  , bound :: CoordinateSystem Point -> Point
  }

data ActionF m state a
  = RandomNumber Int Int (Int -> a)
  | Lift (m a)
  | Modify (state -> state) (state -> a)
  | Utilities (Utilities -> a)

derive instance functorActionF :: Functor m => Functor (ActionF m state)

newtype Action m state a =
  Action (Free (ActionF m state) a)

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
  tailRecM k a =
    k a >>= case _ of
      Loop x -> tailRecM k x
      Done y -> pure y

utilities :: forall state m. Action m state Utilities

utilities = Action $ liftF $ Utilities identity

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

preventDefaultBehavior ::
  forall state m. Action m { paused :: Boolean | state } DefaultBehavior
preventDefaultBehavior = Action $ liftF $ Modify identity (const Prevent)

executeDefaultBehavior ::
  forall state m. Action m { paused :: Boolean | state } DefaultBehavior
executeDefaultBehavior = Action $ liftF $ Modify identity (const Execute)

evalAction ::
  forall world m a.
  MonadEffect m =>
  { width :: Int, cellSize :: Int, height :: Int } ->
  StateId (State m world) ->
  Action m world a ->
  HookM m a
evalAction { width, cellSize, height } stateId (Action action) =
  foldFree (go (Proxy :: Proxy world)) action

  where
  go ::
    forall f b.
    MonadEffect f =>
    Proxy world ->
    ActionF f world b ->
    HookM f b
  go _ (RandomNumber min max cc) = do
    n <- liftEffect (randomInt min max)
    pure $ cc n
  go _ (Modify modifyWorld cc) = do
    newState <- Hooks.modify stateId \s -> s { world = modifyWorld s.world }
    pure $ cc newState.world
  go _ (Utilities cc) = do
    let
      w = toNumber width
      h = toNumber height
      cs = toNumber cellSize
      clip n b = max (min n b) 0.0
      bound = case _ of
        RelativeToCanvas { x, y } ->
          { x: clip x (cs * w - 0.01), y: clip y (cs * h - 0.01) }
        RelativeToGrid { x, y } -> { x: clip x w, y: clip y h }
    pure $ cc { width, cellSize, height, bound }
  go _ (Lift ma) = lift ma
