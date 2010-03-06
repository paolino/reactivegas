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

layout = [["accesso"],["eventi"],["correzione","eventi"], ["interrogazione"],["amministrazione"]]
pagina b = output . prettyHtml $  
		header << (thelink ! [rel "stylesheet", href "/style.css", thetype "text/css"] << noHtml)
		+++ body << ((thediv ! [theclass "testata"] << "reactivegas (alpha)") +++ (thediv ! [theclass "utente"] << b))


main = do
	c <- atomically newTChan
	forkIO . forever $ (atomically (readTChan c) >>= putStrLn)
	pe <- mkGroupSystem loader nuovoStato c "tarogas" >>= startGroupSystem 10000000
	sessionServer 5000 100  applicazione pagina layout ((,) pe <$> mkSessione) 
	
