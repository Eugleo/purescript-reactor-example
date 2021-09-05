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

data MouseEvent
  = MouseEvent
    { type :: MouseEventType
    , x :: Int
    , y :: Int
    , control :: Boolean
    , meta :: Boolean
    , alt :: Boolean
    , shift :: Boolean
    , button :: Int
    }

foreign import offsetX :: ME.MouseEvent -> Int

foreign import offsetY :: ME.MouseEvent -> Int

fromEvent :: MouseEventType -> Int -> ME.MouseEvent -> MouseEvent
fromEvent eventType cellSize event =
  MouseEvent
    { type: eventType
    , x: offsetX event / cellSize
    , y: offsetY event / cellSize
    , control: ME.ctrlKey event
    , alt: ME.altKey event
    , meta: ME.metaKey event
    , shift: ME.shiftKey event
    , button: ME.button event
    }
