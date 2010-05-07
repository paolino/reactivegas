-- {-# LANGUAGE  #-}

import Control.Applicative ((<$>))
import Control.Concurrent.STM (readTChan,newTChan,atomically)
import Control.Concurrent (forkIO)
import Control.Monad (forever)
import Text.XHtml
import Network.SCGI

import Debug.Trace
-------------------------------------------------------
import Lib.Response

import Core.Types (Esterno,Utente)

import Applicazioni.Reactivegas (QS,loader, caricamento, nuovoStato, maxLevel) 
import Applicazioni.Server (sessionServer)
import Applicazioni.Persistenza (mkPersistenza , Persistenza (readLogs,caricamentoBianco,updateSignal,queryUtente))
import Applicazioni.Sessione (mkSessione)

import UI.Server (applicazione)

layout = 	[(["gestione dichiarazioni"],2)
		,(["descrizione sessione"],1)
		,(["amministrazione"],5)
		,(["effetto delle ultime dichiarazioni"],3)
		,(["interrogazione sullo stato del gruppo"],4)
		]

pagina b = output . prettyHtml $  
		header << ((thelink ! [rel "stylesheet", href "/style.css", thetype "text/css"] << noHtml)
				+++ (thetitle << "Amministrazione G.A.S.") +++ (meta ! [httpequiv "Content-Type", content "text/html;charset=utf8;"]))  
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

	
main = do
	pe <- mkPersistenza loader caricamento nuovoStato "tarogas" 20
	forkIO . forever $ readLogs pe >>= putStrLn
	sessionServer 5000 10 20 applicazione pagina layout ((,) pe <$> mkSessione (caricamentoBianco pe) maxLevel (updateSignal pe) (queryUtente pe)) 
	
