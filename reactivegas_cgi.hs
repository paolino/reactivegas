-- {-# LANGUAGE  #-}

import Control.Applicative ((<$>))
import Control.Concurrent.STM (readTChan,newTChan,atomically)
import Control.Concurrent (forkIO)
import Control.Monad (forever)


import Applicazioni.Server (sessionServer)

import Core.Persistenza (mkGroupSystem, startGroupSystem, readStato)
import Core.Sessione (mkSessione, readEventi , readAccesso)
import Core.UI (applicazione)
import Core.Applicazione (loader, caricamento, nuovoStato) 
import Text.XHtml
import Network.SCGI

layout = [["esecuzione accesso"]
	,["produzione eventi"]
	,["interrogazione"]
	,["amministrazione"]
	,["descrizione sessione"]
	,["effetto del caricamento degli eventi","produzione eventi"]
	,["attribuzione di un voto","eventi democratici","produzione eventi"]
	]
pagina b = output . prettyHtml $  
		header << (thelink ! [rel "stylesheet", href "/style.css", thetype "text/css"] << noHtml +++ thetitle << "Reactivegas") 
		+++ body << ((thediv ! [theclass "testata"] << anchor ! [href "/reset"] << "reactivegas (alpha)") +++ (thediv ! [theclass "utente"] << b))

caricamento' s Nothing _ = (s,"")
caricamento' s _ [] = (s,"")
caricamento' s (Just (u,_)) evs = caricamento (map ((,) u) evs) s
main = do
	c <- atomically newTChan
	forkIO . forever $ (atomically (readTChan c) >>= putStrLn)
	(gs,modif,agg) <- mkGroupSystem loader caricamento' nuovoStato c "tarogas" 
	pe <- startGroupSystem 10000000 gs
	sessionServer 5000 100  applicazione pagina layout ((,) pe <$> mkSessione modif agg) 
	
