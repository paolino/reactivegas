{-# LANGUAGE ViewPatterns, ScopedTypeVariables, NoMonomorphismRestriction #-}
import Prelude hiding (catch)
import Network.Shed.Httpd
import Control.Applicative
import Control.Concurrent.STM
import Control.Parallel.Strategies
import Network.URI
import Data.List
import Data.Maybe
import Control.Monad.Error
import Control.Monad.Reader
import Control.Arrow
import Control.Exception
import Codec.Crypto.RSA
import Text.JSON
import MIME

import qualified Data.Map as M
import qualified Data.Set as S
import Costruzione
import Componenti
import Integrity
import Eventi
import Debug.Trace


load :: Read a => String -> IO a
load s = read <$> (readFile s >>= \e -> rnf e `seq` return e)

instance JSON Logico where
	readJSON _ = Error "no way to parse a logico"
	showJSON x = JSString $ toJSString (show x)
instance JSON Bene where
	readJSON (JSString s) = Ok (Bene $ fromJSString s)
	readJSON _ 		= Text.JSON.Error "failed parsing a Membro"
	showJSON (Bene s) = JSString $ toJSString s

instance JSON Membro where
	readJSON (JSString s) = Ok (Membro $ fromJSString s)
	readJSON _ 		= Text.JSON.Error "failed parsing a Membro"
	showJSON (Membro s) = JSString $ toJSString s
instance JSON PublicKey where
	readJSON _ = Error "no way to parse to a public key"
	showJSON = JSString . toJSString . take 6 . show . public_n


inizia x = isPrefixOf x . uriPath 
positivo = return . Response 200 [] . encode
negativo = return . Response 404 [] . encode

boot = do	evs <- load "logici" :: IO [Logico]
		pu <- read <$> readFile "utente.publ"
		(e,errs) <- espandi (map ((,) pu) evs) <$> read <$> readFile "estratto" 
		return (e,errs)

main = initServer 9090 serve where
	serve r@(Request _ (inizia "/api/estratto" -> True) _ _) = 
		let 	dispatch x z = (\(_,a) -> a z) <$> find (\(l,_) ->  x `isPrefixOf` l) fs 
			fs =  [	
				("/api/estratto/conti_membri", positivo . conti_membri),
				("/api/estratto/membri", positivo . membri),
				("/api/estratto/aperti", positivo . aperti), 
				("/api/estratto/conti_responsabili", positivo . conti_responsabili),
				("/api/estratto/responsabili" ,positivo . responsabili)
				]
		in boot >>= maybe (negativo "domanda non implementata") id . dispatch (uriPath $ reqURI r) . fst
	serve r@(Request _ (inizia "/api/costruzione" -> True) _ _) = do
		(e,errs) <- boot
		case (lookup "costruzione" . queryToArguments . uriQuery . reqURI $ r) >>= parse of 
			Nothing -> negativo "evento non accettato"
			Just (Fine ev) -> do
				writeFile "logici" (show . sort $ ev:map (snd . fst) errs) 
				positivo  "evento inserito"
			Just (Continua ss f) -> positivo $ runReader f e
	
	serve r@(Request _ (inizia "/api/eventi/lista" -> True) _ _) = boot >>= positivo . snd

	serve (Request _ (uriPath -> uri) _ _) =
		let uri' = if uri == "/" then "/index.html" else uri in
		(Response 200 [("Content-type",maybe "text/plain" id (parseExtension uri'))] <$> 
			(print uri' >> readFile ("Pagine" ++ uri')))
			`catch`
		(\(_::IOException) -> return $ Response 404 [] "Errore di IO")
		
