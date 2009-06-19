{-# LANGUAGE TypeOperators, ViewPatterns, TypeSynonymInstances, MultiParamTypeClasses, GeneralizedNewtypeDeriving, ScopedTypeVariables, FlexibleContexts, FlexibleInstances, NoMonomorphismRestriction , Rank2Types #-}
module ClienteGTKLib where

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
import System.Glib.Types

containerEmpty c = G.containerGetChildren c >>= mapM_ (G.containerRemove c)
populateCombo xs c = do
	ls <- G.comboBoxSetModelText c
	mapM_ (G.listStoreAppend ls) (map (take 50) xs)

type GlRet = forall widget. G.WidgetClass widget => (GObject -> widget) -> String -> IO widget
type OP a = Board -> GlRet -> IO a

uiChiavi :: OP ()
uiChiavi _ g = do
	t <- g G.castToEntry "nome del responsabile"
	G.onEntryActivate t $ do
		r <- G.entryGetText t 
		rs <- creaChiaviIO r
		case rs of 
			Left s -> outputlog g s
			Right _ -> outputlog g $ "chiavi create per " ++ r
	return ()		
	p <- g G.castToButton "pulsante crea chiavi di sincronizzazione"
	G.onClicked p $ do
		rs <- creaChiaviIO "sincronizzatore"
		case rs of 
			Left s -> outputlog g s
			Right _ -> outputlog g $ "chiavi create per " ++ "sincronizzatore"

	return ()	

uiConfigurazione :: OP ()
uiConfigurazione b@(Board tc _ _) g = do
	v <-  g G.castToTextView "lista chiavi pubbliche"
	bv <- G.textViewGetBuffer v
	cs <- listaChiaviIO
	G.textBufferSetText bv (unlines $ map fst cs) 
	p <- g G.castToButton "pulsante creazione configurazione"
	t <- g G.castToEntry "indirizzo servente"
	n <- g G.castToEntry "nome del gruppo"
	G.onClicked p $ do
		r <- runErrorT (tagga "lettura parte pubblica della chiave di sincronizzatore" $ 
			catchFromIO (readFile "sincronizzatore.publ") >>= contentReads)
		case r of
			Left s -> outputlog g s
			Right c -> do
				rs <- runErrorT . forM (map snd cs) $ \c -> do 
					tagga ("lettura chiave di responsabile di " ++ c) $ 
						catchFromIO (readFile c) >>= contentReads
				case rs of 
					Left s -> outputlog g s
					Right rs -> do
						r <- G.entryGetText t 
						let s = s0  (zip (map fst cs) rs)
						writeFile "configurazione" $ show (Configurazione (r,9090) s c)
						outputlog g "configurazione creata"
						r <- G.entryGetText n >>= creaGruppo rs s
						case r of 
							Left s -> outputlog g s
							Right _ -> outputlog g "file di gruppo creato"

	rc <- g G.castToButton "pulsante carica configurazione" 
	let cbrc = do
		r <- runErrorT (tagga "lettura configurazione" $ catchFromIO (readFile "configurazione") >>= contentReads)
		case r of
			Left e -> do 
				outputlog g e 
				atomically . writeTVar tc $ Nothing
			Right cf -> do
				atomically . writeTVar tc $ (Just cf)
				outputlog g "configurazione caricata"
				uiSetResponsabili  b g
	G.onClicked rc $ cbrc
	cbrc
	return ()	

uiResponsabile :: OP ()
uiResponsabile b@(Board  _ _ tp) g = do
	resps <- g G.castToComboBox "selezione responsabile"
	G.on resps G.changed $ do
		x <- G.comboBoxGetActive resps
		if  x < 0 then return () else do
			y <- G.comboBoxGetModelText resps >>= flip G.listStoreGetValue x
			z <- cercaChiaveIO y
			case z of 
				Left s -> outputlog g s
				Right k -> do
					(_,es) <- atomically (readTVar tp)
					atomically $ writeTVar tp (Just (y,k),es)
					uiRicaricaPatch b g
	return ()

uiAggiornamento :: OP ()
uiAggiornamento b g = do
	a <- g G.castToButton "pulsante aggiornamento"
	G.onClicked a $ do
		lv <- runReaderT aggiornamentoIO b 
		case lv of 
			Left s -> outputlog g s
			Right t -> do 
				outputlog g (eccoILogs t)
				outputlog g "cliente aggiornato"
				uiSetResponsabili  b g
	return ()

uiRicaricaPatch :: OP ()
uiRicaricaPatch  b g = do
	car <- g G.castToTextView "vista caricamento"
	runReaderT (statoCorrettoIO reattori priorities) b >>= either (outputlog g) (outputcaricamento car . eccoILogs . snd)

uiCostruzioni :: OP ()
uiCostruzioni b@(Board  _ _ tp) g = do 
	de <- g G.castToLabel "domanda evento"
	re <- g G.castToHBox "risposta evento"
	ke <- g G.castToButton "reset evento"
	di <- g G.castToLabel "domanda interrogazione"
	ri <- g G.castToHBox "risposta interrogazione"
	ki <- g G.castToButton "reset interrogazione"
	vistacar <-  g G.castToTextView "vista caricamento"
	l <-  g G.castToTextView "vista log"
	let	runCostruzioneGTK  b d rs c@(Costruzione (Libero s) f) = do
			G.labelSetText d s
			containerEmpty rs
			es <- G.entryNew
			G.containerAdd rs es
			G.widgetShow es
			G.onEntryActivate es $  do
				x <-  G.entryGetText es
				case reads x of 
					[] -> outputlog g ("errore nel leggere il valore " ++ x) 
					[(x,_)] -> f x >>= runCostruzioneGTK b d rs 
			return ()


		runCostruzioneGTK b d rs c@(Costruzione (Scelta s xs) f) = do
			G.labelSetText d s
			containerEmpty rs
			es <- G.comboBoxNew
			populateCombo (map fst xs) es
			G.containerAdd rs es
			G.widgetShow es
			G.on es G.changed $ do
				x <- G.comboBoxGetActive es
				if  x < 0 then return () else f (snd $ xs !! x) >>=  runCostruzioneGTK b d rs 
			return ()


	let 	d x = do
			lv <- runReaderT testAutenticazione b
			case lv of
				Left s ->  outputlog g $ s
				Right _ -> return ()
			atomically $ readTVar tp >>= writeTVar tp . second (x:)
			uiRicaricaPatch b g 
		c = runReaderT (statoCorrettoIO reattori priorities) b 
	evento <- svolgi (interfacciaY "costruzione evento"
		( outputlog g, c, d)	makers) 
	interrogazione <- svolgi (interfacciaY "interrogazione" 
		( outputlog g, c , outputlog g ) queriers) 
	runCostruzioneGTK b de re evento
	runCostruzioneGTK b di ri interrogazione
	G.onClicked ke (runCostruzioneGTK b de re evento)
	G.onClicked ki (runCostruzioneGTK b di ri interrogazione)
	return ()
	where	interfacciaY s (d,q,z) cs = do
			let wrap f k = second r (f k) where
				r c = do
					y <- lift q
					case y of
						Left s -> k s >> return undefined
						Right (s,_) ->  c s >>= lift . z 
			incrocio d s $ map wrap cs
			where 	incrocio d s cs = forever $ callCC dentro where
					dentro ki =  join . parametro . Scelta s $ map ($ ki2) cs where
						ki2 x = lift (d x) >> ki ()

uiSetResponsabili :: OP ()
uiSetResponsabili b@(Board tc ts tp) g = do
	tc <- atomically (readTVar tc)
	c <- g G.castToComboBox "selezione responsabile"
	case tc of 
		Nothing -> outputlog g  $ "la configurazione non é stata caricata"
		Just (Configurazione _ s0 puk) -> do
			((uprk,xs),s') <- atomically (liftM2 (,) (readTVar tp) (readTVar ts))
			let 	s = maybe s0 id s'
			populateCombo (map fst . responsabili . fst $ s) c 

outputlog :: GlRet -> String -> IO ()
outputlog g x = do 
	l <- g G.castToTextView "vista log"
	b <- G.textViewGetBuffer l 
	i <- G.textBufferGetStartIter b 
	G.textIterForwardToEnd i
	G.textBufferInsert b i (x ++ "\n") 
	return ()

outputcaricamento vistacar x = do
	b <- G.textViewGetBuffer vistacar
	G.textBufferSetText b x 


			

	
