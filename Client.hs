{-# LANGUAGE ViewPatterns, ScopedTypeVariables #-}
import Prelude hiding (catch)
import Integrity
import Eventi
import Network.Shed.Httpd
import Control.Applicative
import Control.Concurrent.STM
import Network.URI
import Data.List
import Data.Maybe
import Control.Monad.Error
import Control.Monad
import Control.Exception
import JSON
import Text.JSON

dispatch :: String -> Maybe (Estratto -> String -> String)
dispatch x = snd <$> find (flip isPrefixOf x . fst) fs where
	fs =  [	
		("/api/estratto/conti_membri" , jconti_membri),
		("/api/estratto/membri" , jmembri),
		("/api/estratto/aperti" , japerti), 
		("/api/estratto/conti_responsabili" , jconti_responsabili),
		("/api/estratto/responsabili"  ,jresponsabili)
		]

main = initServer 9090 serve where
	serve r@(Request _ (isPrefixOf "/api/" . uriPath -> True) _ _) = 
		let 	z =  uriPath $ reqURI r 
		in do	estratto <- read <$> readFile "estratto"
			return $ case dispatch z of
				Just ss -> Response 200 [] (ss estratto "")
				Nothing -> Response 404 [] ("funzione non implementata: " ++ z)
	serve (Request _ (uriPath -> uri) _ _) = 
		let k = case uri of 
				"/" -> readFile "Pagine/index.html" >>= 
					return . Response 200 [("Mime-type","text/html")]
				q -> do	l <- readFile $ "Pagine/" ++ q
					let mt = if ".jpg" `isSuffixOf` q then Just ("Content-type","image/jpg")
					  		else Nothing
					return $ case mt of 	Just mt -> Response 200 [mt] l
								Nothing -> Response 404 [] 
									"Estensione del file sconosciuta"
		in catch k (\(_::IOException) -> return $ Response 404 [] "Errore di IO")

