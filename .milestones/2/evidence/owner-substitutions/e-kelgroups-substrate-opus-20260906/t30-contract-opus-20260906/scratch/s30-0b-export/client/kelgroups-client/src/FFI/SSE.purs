module FFI.SSE
  ( EventSource
  , create
  , onMessage
  , close
  ) where

import Prelude

import Effect (Effect)

-- | Opaque EventSource handle.
foreign import data EventSource :: Type

foreign import createImpl :: String -> Effect EventSource
foreign import onMessageImpl
  :: EventSource -> (String -> Effect Unit) -> Effect Unit

foreign import close :: EventSource -> Effect Unit

-- | Create an EventSource connected to the given URL.
create :: String -> Effect EventSource
create = createImpl

-- | Register a handler for "new" events.
onMessage :: EventSource -> (String -> Effect Unit) -> Effect Unit
onMessage = onMessageImpl
