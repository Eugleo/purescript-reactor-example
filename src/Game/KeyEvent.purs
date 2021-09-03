module Game.KeyEvent where

import Web.UIEvent.KeyboardEvent as KE

data KeyEvent
  = KeyEvent String { shift :: Boolean, control :: Boolean, alt :: Boolean, meta :: Boolean }

fromEvent :: KE.KeyboardEvent -> KeyEvent
fromEvent event =
  KeyEvent (KE.key event)
    { shift: KE.shiftKey event
    , control: KE.ctrlKey event
    , alt: KE.altKey event
    , meta: KE.metaKey event
    }
