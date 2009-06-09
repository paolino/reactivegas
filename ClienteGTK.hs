{-# LANGUAGE TypeOperators, ViewPatterns, TypeSynonymInstances, MultiParamTypeClasses, GeneralizedNewtypeDeriving, ScopedTypeVariables, FlexibleContexts, FlexibleInstances, NoMonomorphismRestriction #-}
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
import qualified Graphics.UI.Gtk as G
import qualified Graphics.UI.Gtk.Glade as Gl
import qualified Graphics.UI.Gtk.TreeList.ListStore as Gls

main =	do
	print "lettura cartella ............"
	b <- throwLefts $ do 
		c <- tagga "lettura configurazione" $ catchFromIO (readFile "configurazione") >>= contentReads 
		s <- tagga "lettura file di stato" $ catchFromIO (readFile "stato" >>= \x -> length x `seq` return x ) >>= contentReads >>= lift . atomically . newTVar
		puk <- tagga "lettura chiave pubblica sincronizzatore" $ catchFromIO (readFile "sincronizzatore.publ") >>= contentReads
		pa <- lift . atomically . newTVar =<< do 	t <- lift . runErrorT $ tagga "lettura patch" $ catchFromIO (readFile "patch") >>= contentReads 
								either (\e -> liftIO (print e) >> return (Nothing, [])) return t
		return $ Board c puk s pa 
	
	G.initGUI
	glade <- maybe (error "no glade!") id <$> Gl.xmlNew "cliente.glade"
	let getWidget = Gl.xmlGetWidget glade
	window  <- Gl.xmlGetWidget glade G.castToWindow "window1"
	G.onDestroy window G.mainQuit
	
	getWidget G.castToButton "pulsante fine" >>= flip G.onClicked (G.widgetDestroy window)
	
	vistalog <-  getWidget G.castToTextView "vista log"

	sinc <- getWidget G.castToButton "pulsante sincronizzazione" 
	
	G.onClicked sinc $ runReaderT sincronizzaIO b >>= either (outputlog vistalog) (outputlog vistalog)

	s <- getWidget G.castToButton "pulsante spedizione" 
	G.onClicked s $ runReaderT spedizionePatchIO b >>= either (outputlog vistalog) (outputlog vistalog)

	resps <- getWidget G.castToComboBox "selezione responsabile"
	runReaderT (responsabiliGTK resps) b

	G.on resps G.changed $ do
		x <- G.comboBoxGetActive resps
		if  x < 0 then return () else do
			y <- G.comboBoxGetModelText resps >>= flip G.listStoreGetValue x
			z <- cercaChiaveIO y
			case z of 
				Left s -> outputlog vistalog s
				Right k -> do
					let (Board _ _ _ tp) = b 
					(_,es) <- atomically (readTVar tp)
					atomically $ writeTVar tp (Just (y,k),es)
			
	vistacar <-  getWidget G.castToTextView "vista caricamento"
	
	a <- getWidget G.castToButton "pulsante aggiornamento"

	G.onClicked a $ do
		l <- runReaderT aggiornamentoIO b 
		case l of 
			Left s -> outputlog vistalog s
			Right t -> do 
				outputcaricamento vistacar (eccoILogs t)
				outputlog vistalog "cliente aggiornato"
				runReaderT (responsabiliGTK resps) b
	
	runProgram b (costruzioneGTK glade)			
	G.widgetShowAll window
	G.mainGUI

populateCombo xs c = do
	ls <- G.comboBoxSetModelText c
	mapM_ (G.listStoreAppend ls) xs

costruzioneGTK l = do 
	
	(d,rs,vistacar,vistalog,reset) <- liftIO $ do
		d <- Gl.xmlGetWidget l G.castToLabel "label domanda"
		rs <- Gl.xmlGetWidget l G.castToHBox "scatola risposta"
		vistacar <-  Gl.xmlGetWidget l G.castToTextView "vista caricamento"
		vistalog <-  Gl.xmlGetWidget l G.castToTextView "vista log"
		reset <- Gl.xmlGetWidget l G.castToButton "pulsante reset evento"
		return (d,rs,vistacar,vistalog,reset)	
	let c = do 
		(r,ls) <- statoCorrettoIO reattori priorities 
		liftIO . outputcaricamento vistacar . eccoILogs $ ls
		return r
	return ()
	base <- svolgi (creazioneEvento (liftIO . outputlog vistalog, c) makers) 
	runCostruzioneGTK d rs vistalog base
	r <- ask
	liftIO $ G.onClicked reset (runProgram r (runCostruzioneGTK d rs vistalog base))

creazioneEvento (d,q) cs = do
	let 	
		wrap f k = let 	(s,c) = f k 
				y = do	r <- lift q				
					c r >>= z
				in (s,y) 
			
	incrocio d "creazione evento" $ map wrap cs
	where 	z x = lift . modify . second $ (x:)
		incrocio d s cs = callCC $ forever . dentro where
			dentro ki =  join . parametro . Scelta s $ map ($ ki2) cs where
				ki2 x = lift (d x) >> ki ()

containerEmpty c = G.containerGetChildren c >>= mapM_ (G.containerRemove c)

runCostruzioneGTK d rs  vistalog c@(Costruzione (Libero s) f) = do
	r <- ask
	liftIO $ do 
		G.labelSetText d s
		containerEmpty rs
		es <- G.entryNew
		G.containerAdd rs es
		G.widgetShow es
		G.onEntryActivate es $  do
			x <-  G.entryGetText es
			case reads x of 
				[] -> outputlog vistalog ("errore nel leggere il valore " ++ x) 
				[(x,_)] -> runProgram  r (f x >>= runCostruzioneGTK d rs vistalog) 
		return ()

runCostruzioneGTK d rs vistalog c@(Costruzione (Scelta s xs) f) = do
	r <- ask
	liftIO $ do
		G.labelSetText d s
		containerEmpty rs
		es <- G.comboBoxNew
		populateCombo (map fst xs) es
		G.containerAdd rs es
		G.widgetShow es
		G.on es G.changed $ do
			x <- G.comboBoxGetActive es
			if  x < 0 then return () else runProgram r (f (snd $ xs !! x) >>=  runCostruzioneGTK d rs vistalog)
		return ()


responsabiliGTK c = do
	b@(Board _ g ts tp) <- ask
	((uprk,xs),s) <- liftIO $ atomically (liftM2 (,) (readTVar tp) (readTVar ts))
	liftIO $ populateCombo (map fst . responsabili . fst $ s) c

outputlog vistalog x = do 
	b <- G.textViewGetBuffer vistalog 
	i <- G.textBufferGetStartIter b 
	G.textIterForwardToEnd i
	G.textBufferInsert b i (x ++ "\n") 
outputcaricamento vistacar x = do
	print "here"
	b <- G.textViewGetBuffer vistacar
	G.textBufferSetText b x 


			

	
