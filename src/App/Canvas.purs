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
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (throw)
import Game.Color (gray100)
import Game.Config (Config)
import Game.Grid (Grid)
import Game.Grid as Grid
import Game.KeyEvent (fromEvent) as KeyEvent
import Game.MouseEvent (MouseEventType(..))
import Game.MouseEvent (fromEvent) as MouseEvent
import Graphics.CanvasAction (class CanvasStyle, class MonadCanvasAction, fillRect, filled, launchCanvasAff_, setFillStyle)
import Graphics.CanvasAction as Canvas
import Graphics.CanvasAction.Path (FillRule(..), arcBy_, fill, moveTo, runPath)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import Halogen.Query.Event (eventListener)
import Web.Event.Event as E
import Web.HTML (window) as Web
import Web.HTML.HTMLCanvasElement as HTMLCanvasElement
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window (document) as Web
import Web.HTML.Window (requestAnimationFrame)
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent.EventTypes as KET
import Web.UIEvent.MouseEvent as ME
import Web.UIEvent.MouseEvent.EventTypes as MET

canvasId :: String
canvasId = "canvas"

enumerate :: forall a. Array a -> Array (Tuple Int a)
enumerate xs = zip (0 .. (length xs - 1)) xs

withJust :: forall a m. Applicative m => Maybe a -> (a -> m Unit) -> m Unit
withJust (Just x) f = f x

withJust Nothing _ = pure unit

component :: forall s q i o m. MonadEffect m => Eq s => Config s -> H.Component q i o m
component { title, init, draw, onKey, onMouse, onTick } =
  Hooks.component \_ _ -> Hooks.do
    _ /\ worldId <- Hooks.useState { context: Nothing, mouseButtonPressed: false, state: init }

    Hooks.useLifecycleEffect $
      (Canvas.getCanvasElementById canvasId) >>= case _ of
        Nothing ->
          liftEffect $ throw $ "Critical error: No canvas with id " <> canvasId <> " found"
        Just canvas -> do
          context <- Canvas.getContext2D canvas
          renderGrid (draw init) context
          Hooks.modify_ worldId \s -> s { context = Just context }
          document <- liftEffect $ Web.document =<< Web.window

          withJust onTick \onTickEvent -> do
            window <- liftEffect $ Web.window
            animationFrameId <- liftEffect $
              requestAnimationFrame (ticker onTickEvent context worldId) window
            pure unit

          withJust onKey \onKeyEvent ->
            Hooks.subscribe' \_ ->
              eventListener
                KET.keydown
                (HTMLDocument.toEventTarget document)
                (map (handleKey context onKeyEvent worldId) <<< KE.fromEvent)

          withJust onMouse \onMouseEvent -> do
            let
              handle eventType =
                map (handleMouse eventType context onMouseEvent worldId)
                  <<< ME.fromEvent
              listener event eventType =
                eventListener event (HTMLCanvasElement.toEventTarget canvas) (handle eventType)
            Hooks.subscribe' \_ ->
              listener MET.mousedown (const ButtonDown)
            Hooks.subscribe' \_ ->
              listener MET.mouseup (const ButtonUp)
            Hooks.subscribe' \_ ->
              listener MET.mousemove (\w -> if w.mouseButtonPressed then Drag else Move)
            Hooks.subscribe' \_ ->
              listener MET.mouseenter (const Enter)
            Hooks.subscribe' \_ ->
              listener MET.mouseleave (const Leave)
          pure Nothing

    Hooks.pure
      $ HH.div
        [ HP.classes [ H.ClassName "m-auto p-16" ] ]
        [ HH.h1 [ HP.classes [ H.ClassName "text-3xl font-bold mb-8" ] ] [ HH.text title ]
        , HH.canvas
            [ HP.classes
                [ H.ClassName "rounded-lg" ]
            , HP.id_ canvasId
            , HP.width 1080
            , HP.height 720
            ]
        ]
  where
  ticker onTickEvent context worldId = do
    world <- Hooks.get worldId
    let newState = onTickEvent world.state
    Hooks.modify_ worldId \w -> w { state = newState }
    when (world.state /= newState) do
      Hooks.modify_ worldId \w -> w { state = newState }
      renderGrid (draw newState) context

  handleKey context onKeyEvent worldId event = do
    world <- Hooks.get worldId
    liftEffect $ E.preventDefault (KE.toEvent event)
    let newState = onKeyEvent world.state (KeyEvent.fromEvent event)
    Hooks.modify_ worldId \w -> w { state = newState }
    when (world.state /= newState) do
      Hooks.modify_ worldId \w -> w { state = newState }
      renderGrid (draw newState) context

  handleMouse getEventType context onMouseEvent worldId event = do
    world <- Hooks.get worldId
    let eventType = getEventType world
    when (eventType == ButtonDown) $
      Hooks.modify_ worldId \w -> w { mouseButtonPressed = true }
    when (eventType == ButtonUp) $
      Hooks.modify_ worldId \w -> w { mouseButtonPressed = false }
    liftEffect $ E.preventDefault (ME.toEvent event)
    let newState = onMouseEvent world.state (MouseEvent.fromEvent eventType 30 event)
    when (world.state /= newState) do
      Hooks.modify_ worldId \w -> w { state = newState }
      renderGrid (draw newState) context

renderGrid :: forall m. MonadEffect m => Grid -> Canvas.Context2D -> m Unit
renderGrid grid context =
  for_ (enumerate grid) \(x /\ col) ->
    for_ (enumerate col) \(y /\ cell) -> case cell of
      Grid.Empty -> liftEffect $ launchCanvasAff_ context do
        setFillStyle gray100
        fillRect
          { height: 30.0
          , width: 30.0
          , x: toNumber (x * 30)
          , y: toNumber (y * 30)
          }
      Grid.Filled color -> liftEffect $ launchCanvasAff_ context do
        drawRoundedRectangle
          { height: 30.0
          , width: 30.0
          , x: toNumber (x * 30)
          , y: toNumber (y * 30)
          }
          color
          8.0

drawRoundedRectangle
  ∷ forall region m color
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
