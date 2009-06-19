{-# LANGUAGE TypeOperators, ViewPatterns, TypeSynonymInstances, MultiParamTypeClasses, GeneralizedNewtypeDeriving, ScopedTypeVariables, FlexibleContexts, FlexibleInstances, NoMonomorphismRestriction , Rank2Types #-}
import System.Environment
import Control.Applicative ((<$>))
import System.IO
import Codec.Crypto.RSA
import Data.Digest.Pure.SHA
import qualified Data.ByteString.Lazy.Char8 as B
import Control.Monad.State
import Control.Monad.Cont
import Control.Monad.Identity
import Data.List
import Control.Arrow
import Data.Maybe
import Control.Monad.Error
import Codec.Binary.UTF8.String
import Data.IORef
import System.Directory
import System.Random
import Network

import Core (Reazione)
import Anagrafe
import Costruzione
import Rete
import Controllo 
import Applicazione
import Prioriti
import ClientLib
import Control.Monad.Reader
import Control.Concurrent.STM
import Control.Concurrent
import qualified Graphics.UI.Gtk as G
import qualified Graphics.UI.Gtk.Glade as Gl
import qualified Graphics.UI.Gtk.TreeList.ListStore as Gls
import System.Glib.Types
import ClienteGTKLib

main =	do
	G.initGUI
	
	glade <- maybe (error "manca il file di descrizione interfaccia") id <$> Gl.xmlNew "cliente.glade"
	
	let g = Gl.xmlGetWidget glade
	
	window  <- Gl.xmlGetWidget glade G.castToWindow "window1"

	G.onDestroy window G.mainQuit	
	g G.castToButton "pulsante fine" >>= flip G.onClicked (G.widgetDestroy window)
	
	tc <- atomically $ newTVar Nothing
 
	st <- runErrorT (tagga "lettura file di stato" $ catchFromIO (readFile "stato" >>= \x -> length x `seq` return x ) >>= contentReads)
		>>= either (\ e -> outputlog g e >> return Nothing) (return . Just) >>= atomically . newTVar 
	
	t <- runErrorT (tagga "lettura patch" $ catchFromIO (readFile "patch") >>= contentReads) >>= 
			either (\ e -> outputlog g e >> return (Nothing, [])) return >>= atomically . newTVar 
	let b = Board tc st t
	ls <- uiListaChiavi g

	uiChiavi b g 
	uiConfigurazione ls b g 
	uiResponsabile b g
	uiAggiornamento b g
	uiSetResponsabili b g
	uiCostruzioni b g
	g G.castToButton "aggiorna lista chiavi" >>= flip G.onClicked (uiAggiornaListaChiavi ls g >> return ())
	g G.castToButton "pulsante sincronizzazione" >>= flip G.onClicked 
		(runReaderT sincronizzaIO b >>= either (outputlog g) (outputlog g))

	g G.castToButton "pulsante spedizione" >>= flip G.onClicked 
		(runReaderT spedizionePatchIO b >>= either (outputlog g) (outputlog g))

	G.widgetShowAll window
	G.mainGUI


			

	
