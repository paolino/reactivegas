module JSON where

import Eventi
import Control.Arrow
import qualified Data.Map as M
import qualified Data.Set as S
import Text.JSON
import Data.Maybe

jconti_membri = showJSObject . toJSObject  . (map $ unMembro *** showJSON) . M.toList . conti_membri
jconti_responsabili = showJSObject . toJSObject  . (map $ prettyResponsabile *** showJSON) . M.toList . conti_responsabili
jmembri = showJSArray  . map (showJSON . unMembro)  . S.toList . membri
japerti = showJSArray  . map (showJSON . unBene)  . M.keys . aperti
jresponsabili = showJSObject . toJSObject  . (map (prettyResponsabile *** (showJSON . unMembro . fromJust)) . filter (isJust . snd)) . 
	M.toList . responsabili



