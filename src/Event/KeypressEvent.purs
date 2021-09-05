module Event.KeypressEvent where

import Web.UIEvent.KeyboardEvent as KE

data KeypressEvent
  = KeypressEvent String
  { shift :: Boolean
  , control :: Boolean
  , alt :: Boolean
  , meta :: Boolean
  }

fromEvent :: KE.KeyboardEvent -> KeypressEvent
fromEvent event =
  KeypressEvent (KE.key event)
    { shift: KE.shiftKey event
    , control: KE.ctrlKey event
    , alt: KE.altKey event
    , meta: KE.metaKey event
    }
