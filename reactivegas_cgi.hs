-- {-# LANGUAGE  #-}

import Control.Applicative ((<$>))
import Control.Concurrent.STM (readTChan,newTChan,atomically)
import Control.Concurrent (forkIO)
import Control.Monad (forever)

import Lib.Response

import Applicazioni.Server (sessionServer)

import Core.Types (Esterno,Utente)
import Core.Persistenza (mkGroupSystem, startGroupSystem, readStato)
import Core.Sessione (mkSessione, readEventi , readAccesso)
import Core.UI (applicazione)
import Core.Applicazione (QS,loader, caricamento, nuovoStato, maxLevel) 
import Text.XHtml
import Network.SCGI

import Debug.Trace

layout = 	[(["gestione dichiarazioni"],2)
		,(["descrizione sessione"],1)
		,(["amministrazione"],5)
		,(["effetto delle ultime dichiarazioni"],3)
		,(["interrogazione sullo stato del gruppo"],4)
		]
pagina b = output . prettyHtml $  
		header << (thelink ! [rel "stylesheet", href "/style.css", thetype "text/css"] << noHtml 
				+++ thetitle << "Amministrazione G.A.S.") 
		+++ body << thediv ! [theclass "testata"] 
				<< 	(thediv ! [theclass "titolo"] 
						<< ("Amministrazione economica del gruppo d'acquisto" +++ anchor !
						[href "http://googlegroups.com/group/tarogas"] << bold << " tarogas ") +++
				 		(thediv ! [theclass "reset"] 
							<< anchor ! [href "/reset"] << "annulla la sessione"
						)
					)
			+++ (thediv ! [theclass "utente"] << b)
			+++ (thediv ! [theclass "pedata"] << ulist << [
				li << ("Servizio sviluppato da " +++ anchor ! 
					[href "mailto:paolo.veronelli@gmail.com"] << "paolino" +++ "e amici"),
				li << ("Codice disponibile sotto licenza BSD presso " +++ anchor ! 
					[href "http://github.com/paolino/reactivegas"] << "github.com")
				])  

caricamento' :: Int -> QS -> [Esterno Utente] -> (QS,Response)
caricamento' l s es = let
	(s',qs) = caricamento l es s
	qs' = ResponseMany (map ResponseOne $ lines qs)
	in (s',qs')
	
main = do
	c <- atomically newTChan
	forkIO . forever $ atomically (readTChan c)
	(gs,modif,agg) <- mkGroupSystem loader caricamento' nuovoStato c "tarogas" 
	pe <- startGroupSystem 10000000 gs
	sessionServer 5000 10 20 applicazione pagina layout ((,) pe <$> mkSessione modif maxLevel agg) 
	
