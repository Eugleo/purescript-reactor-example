module App.Canvas where

import Prelude
import Color (toHexString)
import Data.Array (length, zip, zipWith, (..))
import Data.Foldable (for_)
import Data.Int (fromNumber, toNumber)
import Data.Maybe (Maybe(..), fromJust, isJust)
import Data.String as String
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (log)
import Game.Config (Config)
import Game.Grid (Grid)
import Game.Grid as Grid
import Game.Key (Key, eventToKey)
import Graphics.Canvas (Context2D, fillRect, fillText, getCanvasElementById, getContext2D, setFillStyle)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import Halogen.Query.Event (eventListener)
import Web.Event.Event as E
import Web.HTML (window) as Web
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window (document) as Web
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent.EventTypes as KET

canvasId :: String
canvasId = "canvas"

enumerate :: forall a. Array a -> Array (Tuple Int a)
enumerate xs = zip (0 .. (length xs - 1)) xs

withJust :: forall a b m. Applicative m => Maybe a -> (a -> m Unit) -> m Unit
withJust (Just x) f = f x

withJust Nothing _ = pure unit

component :: forall s q i o m. MonadEffect m => Config s -> H.Component q i o m
component { init, draw, onKey } =
  Hooks.component \_ _ -> Hooks.do
    _ /\ worldId <- Hooks.useState { context: Nothing, state: init }
    Hooks.useLifecycleEffect do
      canvas <- liftEffect $ getCanvasElementById canvasId
      case canvas of
        Nothing -> pure Nothing
        Just cv -> do
          context <- liftEffect $ getContext2D cv
          liftEffect $ renderGrid (draw init) context
          Hooks.modify_ worldId \s -> s { context = Just context }
          document <- liftEffect $ Web.document =<< Web.window
          withJust onKey \onKey ->
            Hooks.subscribe' \_ ->
              eventListener
                KET.keydown
                (HTMLDocument.toEventTarget document)
                (map (handleKey context onKey worldId) <<< KE.fromEvent)
          pure Nothing
    Hooks.pure
      $ HH.div
          [ HP.classes [ H.ClassName "bg-gray-100 rounded-xl flex justify-center items-center" ] ]
          [ HH.canvas [ HP.id_ canvasId, HP.width 1080, HP.height 720 ] ]
  where
  handleKey context onKey worldId event = do
    world <- Hooks.get worldId
    liftEffect $ E.preventDefault (KE.toEvent event)
    let
      newState = onKey world.state (eventToKey event)
    log (KE.key event)
    Hooks.modify_ worldId \w -> w { state = newState }
    liftEffect $ renderGrid (draw newState) context

renderGrid :: Grid -> Context2D -> Effect Unit
renderGrid grid context =
  for_ (enumerate grid) \(x /\ col) ->
    for_ (enumerate col) \(y /\ cell) -> case cell of
      Grid.Empty -> do
        setFillStyle context "black"
        fillRect context { height: toNumber 30, width: toNumber 30, x: toNumber (x * 30), y: toNumber (y * 30) }
      Grid.Filled color -> do
        setFillStyle context (toHexString color)
        fillRect context { height: toNumber 30, width: toNumber 30, x: toNumber (x * 30), y: toNumber (y * 30) }
