module App.Canvas where

import Prelude

import Data.Array (length, zip, (..))
import Data.Foldable (for_)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Data.Vector.Polymorphic (Rect(..), (><))
import Data.Vector.Polymorphic.Class (class ToRegion, toRegion)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (log)
import Effect.Exception (throw)
import Event.KeypressEvent (KeypressEvent)
import Event.KeypressEvent (fromEvent) as KeypressEvent
import Event.MouseEvent (MouseEvent, MouseEventType(..))
import Event.MouseEvent (fromEvent) as MouseEvent
import Event.TickEvent (TickEvent(..))
import Event.TickEvent as TickEvent
import Game.Action (Action, evalAction, get)
import Game.Color (gray100)
import Game.Config (Config)
import Game.DefaultBehavior (DefaultBehavior, optionallyPreventDefault)
import Game.DefaultBehavior as DefaultBehavior
import Game.Grid (Grid)
import Game.Grid as Grid
import Graphics.CanvasAction (class CanvasStyle, class MonadCanvasAction, Context2D, clearRect, fillRect, fillRectFull, filled, launchCanvasAff_, setFillStyle)
import Graphics.CanvasAction as Canvas
import Graphics.CanvasAction.Path (FillRule(..), arcBy_, fill, moveTo, runPath)
import Halogen (modify_)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Hooks (HookM, StateId)
import Halogen.Hooks as Hooks
import Halogen.Query.Event (eventListener)
import Halogen.Subscription (Listener, create, notify, subscribe)
import Web.DOM.Document (doctype)
import Web.Event.Event (preventDefault)
import Web.Event.Event as E
import Web.HTML (Window)
import Web.HTML (window) as Web
import Web.HTML.HTMLCanvasElement as HTMLCanvasElement
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window (document, requestAnimationFrame) as Web
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent.EventTypes as KET
import Web.UIEvent.MouseEvent as ME
import Web.UIEvent.MouseEvent.EventTypes as MET

canvasId :: String
canvasId = "canvas"

enumerate :: forall a. Array a -> Array (Tuple Int a)
enumerate xs = zip (0 .. (length xs - 1)) xs

withJust :: forall a b m. Applicative m => Maybe a -> (a -> m b) -> m Unit
withJust (Just x) f = f x *> pure unit

withJust Nothing _ = pure unit

type Internal m state =
  { context :: Maybe Context2D
  , mouseButtonPressed :: Boolean
  , draw :: state -> Grid
  , onTick :: TickEvent -> Action m state Unit
  , onKey :: KeypressEvent -> Action m state DefaultBehavior
  , onMouse :: MouseEvent -> Action m state DefaultBehavior
  , renderListener :: Maybe (Listener (HookM m Unit))
  , lastTick :: Number
  , lastGrid :: Maybe Grid
  }

requestGridRerender
  :: forall state m
   . MonadEffect m
  => StateId (Internal m state)
  -> StateId state
  -> HookM m Unit
requestGridRerender internalId worldId = do
  { renderListener } <- Hooks.get internalId
  withJust renderListener \listener -> do
    window <- liftEffect Web.window
    _ <- liftEffect $ Web.requestAnimationFrame
      (renderGrid internalId worldId listener)
      window
    pure unit

component :: forall s q i o m. MonadEffect m => Config m { paused :: Boolean | s } -> H.Component q i o m
component { title, init, draw, onKey, onMouse, onTick } =
  Hooks.component \_ _ -> Hooks.do
    _ /\ worldId <- Hooks.useState init
    _ /\ internalId <- Hooks.useState
      { context: Nothing
      , renderListener: Nothing
      , mouseButtonPressed: false
      , draw
      , onKey
      , onMouse
      , onTick
      , lastTick: 0.0
      , lastGrid: Nothing
      }

    Hooks.useLifecycleEffect $
      (Canvas.getCanvasElementById canvasId) >>= case _ of
        Nothing ->
          liftEffect $ throw $ "Critical error: No canvas with id " <> canvasId <> " found"
        Just canvas -> do
          context <- Canvas.getContext2D canvas
          Hooks.modify_ internalId \s -> s { context = Just context }

          setupRedrawEvents internalId
          requestGridRerender internalId worldId

          setupKeyEvents internalId worldId
          setupTickEvents internalId worldId
          setupMouseEvents internalId worldId canvas

          pure Nothing

    Hooks.pure
      $ HH.div
        [ HP.classes [ H.ClassName "m-auto p-16" ] ]
        [ HH.h1 [ HP.classes [ H.ClassName "text-3xl font-bold mb-8" ] ] [ HH.text title ]
        , HH.canvas
            [ HP.classes
                [ H.ClassName "rounded-lg bg-gray-100" ]
            , HP.id_ canvasId
            , HP.width 1080
            , HP.height 720
            ]
        ]
  where
  setupRedrawEvents internalId = do
    { emitter, listener } <- liftEffect create
    Hooks.subscribe' $ const emitter
    Hooks.modify_ internalId \s -> s { renderListener = Just listener }
    pure unit

  setupKeyEvents internalId worldId = do
    document <- liftEffect $ Web.document =<< Web.window
    Hooks.subscribe' \_ ->
      eventListener
        KET.keydown
        (HTMLDocument.toEventTarget document)
        (map (handleKey internalId worldId) <<< KE.fromEvent)

  setupMouseEvents internalId worldId canvas = do
    let
      target = HTMLCanvasElement.toEventTarget canvas
      handle gameEvent =
        map (handleMouse internalId worldId gameEvent) <<< ME.fromEvent
      subscribe domEvent gameEvent = Hooks.subscribe' \_ ->
        eventListener domEvent target (handle gameEvent)
    subscribe MET.mousedown (const ButtonDown)
    subscribe MET.mouseup (const ButtonUp)
    subscribe MET.mousemove (if _ then Drag else Move)
    subscribe MET.mouseenter (const Enter)
    subscribe MET.mouseleave (const Leave)

  setupTickEvents internalId worldId = do
    { emitter, listener } <- liftEffect create
    window <- liftEffect $ Web.window
    Hooks.subscribe' $ const emitter
    _ <- liftEffect $
      Web.requestAnimationFrame
        (handleTick internalId worldId listener)
        window
    pure unit

handleMouse
  :: forall m state
   . MonadEffect m
  => StateId (Internal m state)
  -> StateId state
  -> (Boolean -> MouseEventType)
  -> ME.MouseEvent
  -> HookM m Unit
handleMouse internalId worldId getEventType event = do
  { mouseButtonPressed, onMouse } <- Hooks.get internalId
  let eventType = getEventType mouseButtonPressed
  when (eventType == ButtonDown) $
    Hooks.modify_ internalId \s -> s { mouseButtonPressed = true }
  when (eventType == ButtonUp) $
    Hooks.modify_ internalId \s -> s { mouseButtonPressed = false }
  defaultBehavior <- evalAction worldId (onMouse (MouseEvent.fromEvent eventType 30 event))
  optionallyPreventDefault defaultBehavior (ME.toEvent event)
  requestGridRerender internalId worldId

handleKey
  :: forall m state
   . MonadEffect m
  => StateId (Internal m state)
  -> StateId state
  -> KE.KeyboardEvent
  -> HookM m Unit
handleKey internalId worldId event = do
  { onKey } <- Hooks.get internalId
  defaultBehavior <- evalAction worldId $ onKey (KeypressEvent.fromEvent event)
  optionallyPreventDefault defaultBehavior (KE.toEvent event)
  requestGridRerender internalId worldId

handleTick
  :: forall m state
   . MonadEffect m
  => StateId (Internal m { paused :: Boolean | state })
  -> StateId { paused :: Boolean | state }
  -> Listener (HookM m Unit)
  -> Effect Unit
handleTick internalId worldId listener =
  notify listener do
    { onTick, lastTick } <- Hooks.get internalId
    { paused } <- Hooks.get worldId
    window <- liftEffect Web.window
    now <- liftEffect $ TickEvent.windowPerformanceNow window
    Hooks.modify_ internalId \s -> s { lastTick = now }
    when (not paused) $ do
      evalAction worldId $ onTick (TickEvent { delta: (now - lastTick) / 1000.0 })
      liftEffect $ renderGrid internalId worldId listener
    _ <- liftEffect $
      Web.requestAnimationFrame
        (handleTick internalId worldId listener)
        window
    pure unit

renderGrid
  :: forall m state
   . MonadEffect m
  => StateId (Internal m state)
  -> StateId state
  -> Listener (HookM m Unit)
  -> Effect Unit
renderGrid internalId worldId listener = do
  notify listener do
    { draw, context, lastGrid } <- Hooks.get internalId
    withJust context \ctx -> do
      world <- Hooks.get worldId
      let grid = draw world
      for_ (enumerate grid) \(x /\ col) ->
        for_ (enumerate col) \(y /\ cell) -> do
          let
            currentValue = Grid.index grid x y
            lastValue = map (\g -> Grid.index g x y) lastGrid
          when (Just (currentValue) /= lastValue) $ do
            case cell of
              Grid.Empty -> liftEffect $ launchCanvasAff_ ctx do
                clearRect
                  { height: 30.0
                  , width: 30.0
                  , x: toNumber (x * 30)
                  , y: toNumber (y * 30)
                  }
              Grid.Filled color -> liftEffect $ launchCanvasAff_ ctx do
                drawRoundedRectangle
                  { height: 30.0
                  , width: 30.0
                  , x: toNumber (x * 30)
                  , y: toNumber (y * 30)
                  }
                  color
                  10.0
            Hooks.modify_ internalId \s -> s { lastGrid = Just grid }

drawRoundedRectangle
  :: forall region m color
   . MonadCanvasAction m
  => ToRegion Number region
  => CanvasStyle color
  => region
  -> color
  -> Number
  -> m Unit
drawRoundedRectangle region color radius = do
  path <- runPath roundedRectPath
  filled color (fill Nonzero path)
  where
  roundedRectPath = do
    let
      (Rect (x >< y) (width >< height)) = toRegion region
      r = min (height / 2.0) (min (width / 2.0) radius)
    moveTo { x: x + r, y }
    arcBy_ { x: width - r, y: 0.0 } { x: 0.0, y: r } r
    arcBy_ { x: 0.0, y: height - r } { x: -r, y: 0.0 } r
    arcBy_ { x: -width + r, y: 0.0 } { x: 0.0, y: -r } r
    arcBy_ { x: 0.0, y: -height + r } { x: r, y: 0.0 } r
