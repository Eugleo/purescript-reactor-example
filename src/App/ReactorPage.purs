module App.ReactorPage (component) where

import Prelude

import Reactor.Properties (Properties) as Reactor
import Reactor.State (State) as Reactor
import Data.Foldable (for_)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Data.Vector.Polymorphic (Rect(..), (><))
import Data.Vector.Polymorphic.Class (class ToRegion, toRegion)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (throw)
import Event.DefaultBehavior (optionallyPreventDefault)
import Event.KeypressEvent (fromEvent) as KeypressEvent
import Event.MouseEvent (MouseEventType(..))
import Event.MouseEvent (fromEvent) as MouseEvent
import Event.TickEvent (TickEvent(..))
import Event.TickEvent as TickEvent
import Reactor.Action (evalAction)
import Reactor (Reactor)
import Graphics.CanvasAction (class CanvasStyle, class MonadCanvasAction, clearRect, filled, launchCanvasAff_)
import Graphics.CanvasAction as Canvas
import Graphics.CanvasAction.Path (FillRule(..), arcBy_, fill, moveTo, runPath)
import Graphics.Drawing (renderDrawing)
import Graphics.Grid (differencesFrom)
import Graphics.Grid as Grid
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Hooks (HookM, StateId)
import Halogen.Hooks as Hooks
import Halogen.Query.Event (eventListener)
import Halogen.Subscription (Listener, create, notify)
import Utilities (withJust)
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

type State m world = StateId (Reactor.State m world)
type Props m world = StateId (Reactor.Properties m world)

component ::
  forall world q i o m.
  MonadEffect m =>
  Reactor m { paused :: Boolean | world } ->
  H.Component q i o m
component { title, init, draw, onKey, onMouse, onTick, width, height } =
  Hooks.component \_ _ -> Hooks.do
    _ /\ stateId <- Hooks.useState
      { context: Nothing
      , renderListener: Nothing
      , mouseButtonPressed: false
      , world: init
      , lastTick: 0.0
      , lastGrid: Nothing
      }
    { cellSize } /\ propsId <- Hooks.useState
      { title
      , draw
      , onKey
      , onMouse
      , onTick
      , width
      , height
      , cellSize: 30
      }

    Hooks.useLifecycleEffect $
      (Canvas.getCanvasElementById canvasId) >>= case _ of
        Nothing ->
          liftEffect $ throw $ "Critical error: No canvas with id " <> canvasId <>
            " found"
        Just canvas -> do
          context <- Canvas.getContext2D canvas
          Hooks.modify_ stateId \s -> s { context = Just context }

          setupRedrawEvents stateId
          requestGridRerender stateId propsId

          setupKeyEvents stateId propsId
          setupTickEvents stateId propsId
          setupMouseEvents stateId propsId canvas

          pure Nothing

    Hooks.pure
      $ HH.div
        [ HP.classes [ H.ClassName "m-auto p-16" ] ]
        [ HH.h1 [ HP.classes [ H.ClassName "text-3xl font-bold mb-8" ] ] [ HH.text title ]
        , HH.canvas
            [ HP.classes
                [ H.ClassName "rounded-lg bg-gray-100" ]
            , HP.id_ canvasId
            , HP.width $ width * cellSize
            , HP.height $ height * cellSize
            ]
        ]
  where
  setupRedrawEvents internalId = do
    { emitter, listener } <- liftEffect create
    Hooks.subscribe' $ const emitter
    Hooks.modify_ internalId \s -> s { renderListener = Just listener }
    pure unit

  setupKeyEvents stateId propsId = do
    document <- liftEffect $ Web.document =<< Web.window
    Hooks.subscribe' \_ ->
      eventListener
        KET.keydown
        (HTMLDocument.toEventTarget document)
        (map (handleKey stateId propsId) <<< KE.fromEvent)

  setupMouseEvents stateId propsId canvas = do
    let
      target = HTMLCanvasElement.toEventTarget canvas
      handle gameEvent =
        map (handleMouse stateId propsId gameEvent) <<< ME.fromEvent
      subscribe domEvent gameEvent = Hooks.subscribe' \_ ->
        eventListener domEvent target (handle gameEvent)
    subscribe MET.mousedown (const ButtonDown)
    subscribe MET.mouseup (const ButtonUp)
    subscribe MET.mousemove (if _ then Drag else Move)
    subscribe MET.mouseenter (const Enter)
    subscribe MET.mouseleave (const Leave)

  setupTickEvents stateId propsId = do
    { emitter, listener } <- liftEffect create
    window <- liftEffect $ Web.window
    Hooks.subscribe' $ const emitter
    _ <- liftEffect $
      Web.requestAnimationFrame
        (handleTick stateId propsId listener)
        window
    pure unit

handleMouse ::
  forall m world.
  MonadEffect m =>
  State m world ->
  Props m world ->
  (Boolean -> MouseEventType) ->
  ME.MouseEvent ->
  HookM m Unit
handleMouse stateId propsId getEventType event = do
  { onMouse } <- Hooks.get propsId
  { mouseButtonPressed } <- Hooks.get stateId
  let eventType = getEventType mouseButtonPressed
  when (eventType == ButtonDown) $
    Hooks.modify_ stateId \s -> s { mouseButtonPressed = true }
  when (eventType == ButtonUp) $
    Hooks.modify_ stateId \s -> s { mouseButtonPressed = false }
  { height, width, cellSize } <- Hooks.get propsId
  defaultBehavior <- evalAction { height, width, cellSize } stateId
    (onMouse (MouseEvent.fromEvent eventType 30 event))
  optionallyPreventDefault defaultBehavior (ME.toEvent event)
  requestGridRerender stateId propsId

handleKey ::
  forall m world.
  MonadEffect m =>
  State m world ->
  Props m world ->
  KE.KeyboardEvent ->
  HookM m Unit
handleKey stateId propsId event = do
  { onKey } <- Hooks.get propsId
  { height, width, cellSize } <- Hooks.get propsId
  defaultBehavior <-
    evalAction { height, width, cellSize } stateId $
      onKey (KeypressEvent.fromEvent event)
  optionallyPreventDefault defaultBehavior (KE.toEvent event)
  requestGridRerender stateId propsId

-- TODO Optimize, do not call when paused
handleTick ::
  forall m world.
  MonadEffect m =>
  State m { paused :: Boolean | world } ->
  Props m { paused :: Boolean | world } ->
  Listener (HookM m Unit) ->
  Effect Unit
handleTick stateId propsId listener =
  notify listener do
    { lastTick, world } <- Hooks.get stateId
    { onTick } <- Hooks.get propsId
    window <- liftEffect Web.window
    now <- liftEffect $ TickEvent.windowPerformanceNow window
    Hooks.modify_ stateId \s -> s { lastTick = now }
    when (not world.paused) $ do
      { height, width, cellSize } <- Hooks.get propsId
      evalAction { height, width, cellSize } stateId $
        onTick (TickEvent { delta: (now - lastTick) / 1000.0 })
      liftEffect $ renderGrid stateId propsId listener
    _ <- liftEffect $
      Web.requestAnimationFrame
        (handleTick stateId propsId listener)
        window
    pure unit

requestGridRerender ::
  forall m world.
  MonadEffect m =>
  State m world ->
  Props m world ->
  HookM m Unit
requestGridRerender stateId propsId = do
  { renderListener } <- Hooks.get stateId
  withJust renderListener \listener -> do
    window <- liftEffect Web.window
    _ <- liftEffect $ Web.requestAnimationFrame
      (renderGrid stateId propsId listener)
      window
    pure unit

renderGrid ::
  forall m world.
  MonadEffect m =>
  State m world ->
  Props m world ->
  Listener (HookM m Unit) ->
  Effect Unit
renderGrid stateId propsId listener = do
  notify listener do
    { context, lastGrid, world } <- Hooks.get stateId
    { width, height, cellSize, draw } <- Hooks.get propsId
    withJust context \ctx -> do
      let grid = renderDrawing (toNumber cellSize) { width, height } $ draw world
      case lastGrid of
        Nothing -> for_ (Grid.enumerate grid) $ renderCell ctx cellSize
        Just reference ->
          for_ (grid `differencesFrom` reference) $ renderCell ctx cellSize
      Hooks.modify_ stateId \s -> s { lastGrid = Just grid }
  where
  renderCell context size (Tuple { x, y } cell) =
    case cell of
      Grid.EmptyCell -> liftEffect $ launchCanvasAff_ context do
        clearRect
          { height: toNumber size
          , width: toNumber size
          , x: toNumber (x * size)
          , y: toNumber (y * size)
          }
      Grid.Colored style -> liftEffect $ launchCanvasAff_ context do
        drawRoundedRectangle
          { height: toNumber size - 2.0
          , width: toNumber size - 2.0
          , x: toNumber (x * size) + 1.0
          , y: toNumber (y * size) + 1.0
          }
          style
          7.0

drawRoundedRectangle ::
  forall region m color.
  MonadCanvasAction m =>
  ToRegion Number region =>
  CanvasStyle color =>
  region ->
  color ->
  Number ->
  m Unit
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
