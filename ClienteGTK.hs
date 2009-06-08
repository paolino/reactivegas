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
import MakePatch 
import Costruzione
import Rete
import Controllo 
import Applicazione
import Prioriti
import Control.Monad.Reader
import Control.Concurrent.STM
import qualified Graphics.UI.Gtk as G
import qualified Graphics.UI.Gtk.Glade as Gl
import qualified Graphics.UI.Gtk.TreeList.ListStore as Gls
tagga x f = f `catchError` (\s -> throwError $ x ++ ":" ++ s)

throwLefts f = do 
	x <- runErrorT f 
	case x of 	Left y -> error y
			Right y -> return y	
catchFromIO f = ErrorT ((Right <$> f) `catch` (return . Left . show))

liftErrorT :: (MonadIO m) => ErrorT e IO a -> ErrorT e m a
liftErrorT f = ErrorT (liftIO (runErrorT f))

contentReads x = case reads x of 	
		[] -> throwError $ "errore nell'interpretare " ++ take 50 (show x)
		[(y,_)] -> return y




data Board = Board Configurazione PublicKey (TVar Q) (TVar Patch)
data Configurazione = Configurazione HostName Integer deriving (Read,Show)


query host p y = do
	l <- liftErrorT . catchFromIO $ do
		h <- connectTo host (PortNumber (fromIntegral p))
		hSetBuffering h NoBuffering
		hPutStrLn h y
		hGetLine h
	case reads l of
		[] -> error "errore di protocollo"
		[(Right l,_)] -> return l
		[(Left l,_)] -> throwError l

query' (Board (Configurazione h p) _ _ _) = query h (fromIntegral p) . show

newtype Program a = Program (ReaderT Board IO a) deriving (Monad, Functor, MonadIO, MonadReader Board )
fromIO = Program . liftIO

runProgram :: Board  -> Program a -> IO a
runProgram b (Program p) = runReaderT p b

instance MonadState Patch Program where
	get = Program $ ask >>= \(Board _ _ _ tp) -> liftIO (atomically $ readTVar tp)
	put x = Program $ ask >>= \(Board _ _ _ tp) -> liftIO (atomically $ writeTVar tp x) 

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
	G.onClicked s $ runReaderT send b >>= either (outputlog vistalog) (outputlog vistalog)

	resps <- getWidget G.castToComboBox "selezione responsabile"
	runReaderT (responsabiliGTK resps) b

	G.on resps G.changed $ do
		x <- G.comboBoxGetActive resps
		if  x < 0 then return () else do
			y <- G.comboBoxGetModelText resps >>= flip G.listStoreGetValue x
			z <- cercaChiave y
			case z of 
				Left s -> outputlog vistalog s
				Right k -> do
					let (Board _ _ _ tp) = b 
					(_,es) <- atomically (readTVar tp)
					atomically $ writeTVar tp (Just (y,k),es)
			
	vistacar <-  getWidget G.castToTextView "vista caricamento"
	
	a <- getWidget G.castToButton "pulsante aggiornamento"

	G.onClicked a $ do
		l <- runReaderT (aggiornamentoIO eccoILogs) b 
		case l of 
			Left s -> outputlog vistalog s
			Right t -> do 
				outputcaricamento vistacar t 
				outputlog vistalog "cliente aggiornato"
				runReaderT (responsabiliGTK resps) b
	
	runProgram b (costruzioneGTK glade)			
	G.widgetShowAll window
	G.mainGUI

populateCombo xs c = do
	ls <- G.comboBoxSetModelText c
	mapM_ (G.listStoreAppend ls) xs

costruzioneGTK l = do
	(d,rs,vistacar,vistalog) <- liftIO $ do
		d <- Gl.xmlGetWidget l G.castToLabel "label domanda"
		rs <- Gl.xmlGetWidget l G.castToHBox "scatola risposta"
		vistacar <-  Gl.xmlGetWidget l G.castToTextView "vista caricamento"
		vistalog <-  Gl.xmlGetWidget l G.castToTextView "vista log"
		return (d,rs,vistacar,vistalog)	
	let c = correggiStato (liftIO . outputcaricamento vistacar . eccoILogs) reattori priorities 
	svolgi (soloCreazioneEvento (liftIO . outputlog vistalog, c) makers) >>= runCostruzioneGTK d rs vistalog 


containerEmpty c = G.containerGetChildren c >>= mapM_ (G.containerRemove c)

runCostruzioneGTK d rs vistalog (Costruzione (Libero s) f) = do
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

runCostruzioneGTK d rs vistalog (Costruzione (Scelta s xs) f) = do
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
			if  x < 0 then return () else runProgram r (f (snd $ xs !! x) >>= runCostruzioneGTK d rs vistalog)
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

creaChiaviIO l = runErrorT . tagga "creazione chiavi in IO" . liftErrorT . catchFromIO $ do
	let (p1,p2) = (l ++ ".priv", l ++ ".publ")
	(pu,pr,_) <- flip generateKeyPair 512 <$> newStdGen
	writeFile p1 (show pr)
	writeFile p2 (show pu)

aggiornamentoIO pl =  runErrorT . tagga "aggiornamentoIO:" $ do
	b@(Board _ g ts tp) <- ask
	((uprk,xs),s) <- liftIO $ atomically (liftM2 (,) (readTVar tp) (readTVar ts))
	let h = showDigest $ sha512 $ B.pack (show s)
	ps <- query' b (g,Aggiornamento h)
	(s',ls) <- aggiornaStato g s ps 
	vs <- query' b (g,Validi)
	when (responsabiliQ s' /= vs) $ throwError "Il sicronizzatore sta truffando sui responsabili validi"
	liftIO $ atomically (writeTVar ts s')
	liftIO $ writeFile "stato" (show s')
	return (pl ls)

sincronizzaIO = runErrorT . tagga "sincronizzazioneIO:" $ do
	b@(Board _ puk ts tp) <- ask
	(_,s) <- liftIO $ atomically (liftM2 (,) (readTVar tp) (readTVar ts))
	let h = showDigest $ sha512 $ B.pack (show s)
	prk <- liftErrorT $ tagga "lettura chiave privata sincronizzatore" $ catchFromIO (readFile "sincronizzatore.priv") >>= contentReads
	ps <-  query' b (puk,UPS)
	let 	ps' = filter (\((pu,firma,es)::UP) -> pu `elem` responsabiliQ s && 
			verify pu (B.pack (h ++ concat es)) firma) ps
		f0 = sign prk (B.pack $ h ++ show ps') 
	(s',ls) <- aggiornaStato puk s [(f0 ,ps')] 
	let 	ws = responsabiliQ $ s'
		f1 = sign prk (B.pack $ h ++ show ws)
		h' = showDigest . sha512 . B.pack $ show s'
	query' b (puk,GroupPatch (h',f0 ,ws, f1))
		

correggiStato :: (MonadIO m, MonadReader Board m)
	=> (Log Utente -> m a)
	-> [Reazione T c Utente]
	-> [R]
	-> m T

correggiStato pl rs bs = do
	(Board _ _ ts tp) <- ask
	((uprk,xs),s) <- liftIO $ atomically (liftM2 (,) (readTVar tp) (readTVar ts))
	let ys = case uprk of 
		Just (u,prk) -> zip (repeat u) xs
		Nothing -> []
	liftIO $ print ys
	case null ys of
		False -> let (r,_,log) = runIdentity $ runProgramma rs s (caricaEventi bs ys >> (fst <$> get)) in
			pl log >> return r
		True -> let (r,_,log) = runIdentity $ runProgramma rs s (fst <$> get) in return r

send  = runErrorT . tagga "spedizione patch di eventi" $ do
	b@(Board _ puk ts tp) <- ask
	((uprk,es),s) <- liftIO $ atomically (liftM2 (,) (readTVar tp) (readTVar ts))
	(u,prk) <- case uprk of 
		Nothing -> throwError "bisogna autenticarsi"
		Just l -> return l
	pu <- liftErrorT $ tagga "lettura chiave pubblica responsabile" $ catchFromIO (readFile $ u ++ ".publ") >>= contentReads
	let 	h = showDigest . sha512 . B.pack . show $ s
	 	r = (pu,sign prk (B.pack $ h ++ concat es),es)
	query' b (puk,Patch r) 

cercaChiave s = runErrorT . tagga ("lettura chiave privata di" ++ decodeString s) $ do
	ls <- liftIO $ getDirectoryContents "."
	case find ((==) $ s ++ ".priv") ls of
		Nothing -> throwError "file assente" 
		Just x -> catchFromIO (readFile x) >>= contentReads

			

	
