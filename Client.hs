{-# LANGUAGE ViewPatterns, ScopedTypeVariables, NoMonomorphismRestriction #-}
import Prelude hiding (catch)
import Control.Exception (catch,IOException)
import Control.Applicative ((<$>))
import Control.Parallel.Strategies (rnf)
import Data.List (isPrefixOf, find, sort, delete)
import Control.Monad.Reader (runReader)
import Network.URI (uriPath,uriQuery)
import Network.Shed.Httpd 
import Text.JSON

import Costruzione
import Componenti
import Integrity
import Eventi
import MIME
-- import Debug.Trace

inizia x = isPrefixOf x . uriPath 
argomento x = lookup x . queryToArguments . uriQuery . reqURI

positivo = return . Response 200 [] . encode
negativo = return . Response 404 [] . encode

boot = do	evs <- load "logici" 
		pu <- load "utente.publ"
		espandi (map ((,) pu) evs) <$> load "estratto" 
	where	load :: Read a => String -> IO a
		load s = read <$> (readFile s >>= \e -> rnf e `seq` return e)

instance JSON Logico where
	readJSON  = undefined
	showJSON = JSString . toJSString . show 

main = initServer 9090 serve where
	serve r@(Request _ (inizia "/api/estratto" -> True) _ _) = 
		boot >>= maybe (negativo "domanda non implementata") id . dispatch (uriPath $ reqURI r) . fst
		where	dispatch x z = (\(_,a) -> a z) <$> find (\(l,_) ->  x `isPrefixOf` l) fs 
			fs =  [	
				("/api/estratto/conti_membri", positivo . conti_membri),
				("/api/estratto/membri", positivo . membri),
				("/api/estratto/aperti", positivo . aperti), 
				("/api/estratto/conti_responsabili", positivo . conti_responsabili),
				("/api/estratto/responsabili" ,positivo . responsabili)
				]
	serve r@(Request _ (inizia "/api/costruzione" -> True) _ _) = do
		(e,errs) <- boot
		case argomento "costruzione" r >>= parse of 
			Nothing -> negativo "evento non accettato"
			Just (Fine ev) -> do
				writeFile "logici" (show . sort $ ev:map (snd . fst) errs) 
				positivo  "evento inserito"
			Just (Continua ss f) -> positivo $ runReader f e
	
	serve r@(Request _ (inizia "/api/eventi/lista" -> True) _ _) = 
		boot >>= positivo . zip [(1::Int) ..] . snd
	serve r@(Request _ (inizia "/api/eventi/cancella" -> True) _ _) = do
		(e,errs) <- boot
		case read <$> argomento "indice" r  of
			Nothing -> negativo "indice non presente"
			Just t -> do
				writeFile "logici" (show . sort . map (snd . fst) $ delete (errs !! t) errs)
				positivo "evento cancellato"
	
	serve r@(Request _ (inizia "/api/eventi/firma" -> True) _ _) =	do
		(e , map (snd . fst) -> ls) <- boot 
		-- pr <- read <$> readFile "utente.priv"
		return $ Response 200 [("Content-type","application/reactivegasrpatch")] $ show ls
			
		


	
	serve (Request _ (uriPath -> uri) _ _) =
		(Response 200 [("Content-type",maybe "text/plain" id (parseExtension uri'))] <$> 
			(print uri' >> readFile ("Pagine" ++ uri')))
			`catch`
		(\(_::IOException) -> return $ Response 404 [] "Errore di IO")
		where uri' = if uri == "/" then "/index.html" else uri 
