module JSON where

import Eventi
import Control.Arrow
import qualified Data.Map as M
import Text.JSON

jconti_membri = toJSObject  . (map $ first unMembro) . M.toList . conti_membri

