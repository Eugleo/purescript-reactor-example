module Event.MouseEvent where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Web.UIEvent.MouseEvent as ME

data MouseEventType
  = ButtonUp
  | ButtonDown
  | Drag
  | Enter
  | Leave
  | Move

derive instance eqMouseEventType :: Eq MouseEventType
derive instance genericMouseEventType :: Generic MouseEventType _
instance showMyADT :: Show MouseEventType where
  show = genericShow

data MouseEvent = MouseEvent
  { type :: MouseEventType
  , x :: Int
  , y :: Int
  , control :: Boolean
  , meta :: Boolean
  , alt :: Boolean
  , shift :: Boolean
  , button :: Int
  }

derive instance genericMouseEvent :: Generic MouseEvent _
instance showMouseEvent :: Show MouseEvent where
  show = genericShow

foreign import offsetX :: ME.MouseEvent -> Int

foreign import offsetY :: ME.MouseEvent -> Int

fromEvent ::
  { cellSize :: Int, width :: Int, height :: Int } ->
  MouseEventType ->
  ME.MouseEvent ->
  MouseEvent
fromEvent { cellSize, width, height } eventType event =
  MouseEvent
    { type: eventType
    , x: clip (offsetX event / cellSize) (height - 1)
    , y: clip (offsetY event / cellSize) (width - 1)
    , control: ME.ctrlKey event
    , alt: ME.altKey event
    , meta: ME.metaKey event
    , shift: ME.shiftKey event
    , button: ME.button event
    }
  where
  clip n b = max (min n b) 0
