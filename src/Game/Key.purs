module Game.Key where

import Web.UIEvent.KeyboardEvent as KE

data Key
  = Key String { shift :: Boolean, control :: Boolean, alt :: Boolean, meta :: Boolean }

eventToKey :: KE.KeyboardEvent -> Key
eventToKey event =
  Key (KE.key event)
    { shift: KE.shiftKey event
    , control: KE.ctrlKey event
    , alt: KE.altKey event
    , meta: KE.metaKey event
    }
