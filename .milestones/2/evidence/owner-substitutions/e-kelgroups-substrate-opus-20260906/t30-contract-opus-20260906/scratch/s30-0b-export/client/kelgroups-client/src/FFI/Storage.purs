module FFI.Storage
  ( getItem
  , setItem
  , removeItem
  , confirm
  , copyToClipboard
  ) where

import Prelude

import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import Effect (Effect)

foreign import getItemImpl :: String -> Effect (Nullable String)

foreign import setItemImpl :: String -> String -> Effect Unit

foreign import removeItemImpl :: String -> Effect Unit

getItem :: String -> Effect (Maybe String)
getItem key = toMaybe <$> getItemImpl key

setItem :: String -> String -> Effect Unit
setItem = setItemImpl

removeItem :: String -> Effect Unit
removeItem = removeItemImpl

foreign import confirmImpl :: String -> Effect Boolean

confirm :: String -> Effect Boolean
confirm = confirmImpl

foreign import copyToClipboardImpl :: String -> Effect Unit

copyToClipboard :: String -> Effect Unit
copyToClipboard = copyToClipboardImpl
