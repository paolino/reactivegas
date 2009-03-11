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

matcher :: [Request -> IO (Maybe Response)] -> Request -> IO Response
matcher zs x = liftM fromJust . foldr1 (liftM2 mplus) . map ($ x) $ zs

main = do
	let	serveDynamic r@(Request _ (isPrefixOf "/api/" . uriPath -> True) _ _) = Just <$> do 
			let z =  uriPath $ reqURI r 
			estratto <- read <$> readFile "estratto"
			case z of	
				(isPrefixOf "/api/estratto/" -> True) -> case z of
					(isPrefixOf "/api/estratto/conti_membri" -> True) ->
						return $ Response 200 [] (show (jconti_membri estratto))
					_ -> return $ Response 404 [] ("non so rispondere a " ++ z)
				_ -> return $ Response 404 [] ("non so rispondere a " ++ z)
		serveDynamic _ = return Nothing
		serveStatic (Request _ (uriPath -> uri) _ _) = 
			let k = do 
				case uri of 
					"/" -> readFile "Pagine/index.html" >>= 
						return . Response 202 [("Mime-type","text/html")]
					q -> do print q
						l <- readFile $ "Pagine/" ++ q 
						let mt = if ".jpg" `isSuffixOf` q then Just ("Content-type","image/jpg")
						  		else Nothing
						return $ case mt of 	Just mt -> Response 200 [mt] l
									Nothing -> Response 404 [] 
										"Estensione del file sconosciuta"
			in Just <$> catch k (\(_::IOException) -> return $ Response 404 [] "Errore di IO")
		serveStatic _ = return Nothing
		wtf _ = return $ Just (Response 404 [] "Senza URI :(")

	initServer 9090 $ matcher [serveDynamic,serveStatic,wtf]

