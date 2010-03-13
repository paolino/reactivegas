-- {-# LANGUAGE  #-}

import Control.Applicative ((<$>))
import Control.Concurrent.STM (readTChan,newTChan,atomically)
import Control.Concurrent (forkIO)
import Control.Monad (forever)


import Applicazioni.Server (sessionServer)

import Core.Types (Esterno,Utente)
import Core.Persistenza (mkGroupSystem, startGroupSystem, readStato)
import Core.Sessione (mkSessione, readEventi , readAccesso)
import Core.UI (applicazione)
import Core.Applicazione (QS,loader, caricamento, nuovoStato) 
import Text.XHtml
import Network.SCGI

layout = 	[["esecuzione accesso"]
		,["produzione eventi"]
		,["interrogazione"]
		,["amministrazione"]
		,["descrizione sessione"]
		]
pagina b = output . prettyHtml $  
		header << (thelink ! [rel "stylesheet", href "/style.css", thetype "text/css"] << noHtml +++ thetitle << "Tarogas (economia)") 
		+++ body 
			<< thediv ! [theclass "testata"] 
				<< (thediv ! [theclass "titolo"] << "Amministrazione distribuita per il gruppo di acquisto Tarogas" +++
				 (thediv ! [theclass "reset"] << anchor ! [href "/reset"] << "reset"))
			+++ (thediv ! [theclass "utente"] << b)
			+++ (thediv ! [theclass "pedata"] << ulist << [
				li << ("Servizio sviluppato da " +++ anchor ! 
					[href "mailto:paolo.veronelli@gmail.com"] << "paolino"),
				li << ("Codice disponibile sotto licenza BSD presso " +++ anchor ! 
					[href "http://github.com/paolino/reactivegas"] << "github.com")
				])  
caricamento' :: QS -> [Esterno Utente] -> (QS,Html)
caricamento' s es = let
	(s',qs) = caricamento es s
	qs' = ulist << map (li <<) (lines qs)
	in (s',qs')
	
main = do
	c <- atomically newTChan
	forkIO . forever $ (atomically (readTChan c) >>= putStrLn)
	(gs,modif,agg) <- mkGroupSystem loader caricamento' nuovoStato c "tarogas" 
	pe <- startGroupSystem 10000000 gs
	sessionServer 5000 100  applicazione pagina layout ((,) pe <$> mkSessione modif agg) 
	
