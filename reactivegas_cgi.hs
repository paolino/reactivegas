-- {-# LANGUAGE  #-}

import Control.Applicative ((<$>))
import Control.Concurrent.STM (readTChan,newTChan,atomically)
import Control.Concurrent (forkIO)
import Control.Monad (forever)
import Text.XHtml
import Network.SCGI

import Debug.Trace
-------------------------------------------------------
import Lib.Server.Server (server)
import Lib.Response

import Core.Types (Esterno,Utente)

import Applicazioni.Reactivegas (QS,loader, caricamento, nuovoStato, maxLevel) 
import Applicazioni.Persistenza (mkPersistenza , Persistenza (readLogs,caricamentoBianco,updateSignal,queryUtente))
import Applicazioni.Sessione (mkSessione)

import UI.Server (applicazione)

layout = 	[(["gestione dichiarazioni"],2)
		,(["descrizione sessione"],1)
		,(["amministrazione"],5)
		,(["effetto delle ultime dichiarazioni"],3)
		,(["interrogazione sullo stato del gruppo"],4)
		]


metadata = header << 	(
				(thelink ! [rel "stylesheet", href "/style.css", thetype "text/css"] << noHtml)
			+++ 	(thetitle << "Amministrazione G.A.S.") 
			+++ 	(meta ! [httpequiv "Content-Type", content "text/html;charset=utf8;"])
			)  
piede = ulist << 	[
			li << ("Servizio sviluppato da " +++ anchor ! [href "mailto:paolo.veronelli@gmail.com"] 
					<< "paolino" +++ "e amici"),
			li << ("Codice disponibile sotto licenza BSD presso " +++ anchor ! 
				[href "http://github.com/paolino/reactivegas"] << "github.com")
			]  

testata n = 	thediv ! [theclass "titolo"] << ("Amministrazione economica del gruppo d'acquisto" +++ n) 
	+++	(thediv ! [theclass "reset"] << anchor ! [href "/"] << "annulla la sessione")

pagina n b = output . prettyHtml $ 
		header << metadata  
	+++ 	body << (		(thediv ! [theclass "testata"] << testata n)
				+++ 	(thediv ! [theclass "utente"] << b)
				+++ 	(thediv ! [theclass "pedata"] << piede)  
			)

	
program 	:: Int 	-- ^ porta cgi
		-> Int 	-- ^ grandezza coda di aggiornamenti di gruppo
		-> Int 	-- ^ numero massimo di ricordi per sessione
		-> Int -- ^ numero massimo di sessioni simultanee 	     
		-> String -- ^ directory di lavoro / nome del gruppo
		-> IO ()
program p  l1 l2 l3 n= do  
	pe <- mkPersistenza loader caricamento nuovoStato n l1
	forkIO . forever $ readLogs pe >>= putStrLn
	server p l2 l3 applicazione (pagina n) layout 
		((,) pe <$> mkSessione (caricamentoBianco pe) maxLevel (updateSignal pe) (queryUtente pe)) 
	
main = program 5000 20 10 20 "tarogas"
