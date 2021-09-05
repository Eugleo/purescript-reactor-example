module Event.TickEvent where

import Effect (Effect)
import Web.HTML (Window)

newtype TickEvent = TickEvent { delta :: Number }

foreign import windowPerformanceNow :: Window -> Effect Number