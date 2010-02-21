
{-# LANGUAGE FlexibleContexts, Rank2Types, ExistentialQuantification, ScopedTypeVariables, GeneralizedNewtypeDeriving, NoMonomorphismRestriction, ImplicitParams #-}
module Core.UI where

import Data.Maybe (fromJust)
import Data.List (delete)

import Control.Arrow
import Control.Applicative
import Control.Monad.Cont
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Error
import System.Console.Haskeline (MonadException)
import Debug.Trace


import Lib.Passo (Costruzione,mano, menu, rotonda ,rmenu, Passo) 
import qualified Lib.Passo as P

import Lib.TreeLogs (eccoILogs)
import Lib.Firmabile (cryptobox)
import Lib.Prioriti (R)
import Lib.Response (Response (..))


import Core.Types (Esterno, Evento)
import Core.Controllo (caricaEventi, SNodo (..))
import Core.Contesto (flatten)
import Core.Programmazione (Reazione)
import Core.Parsing (ParserConRead)
import Core.Patch (login, Firmante (..))
import Core.Costruzione (runSupporto, Supporto)
import Core.Persistenza (Persistenza (..))
import Core.Sessione (Sessione (..))
import Core.Applicazione (QS,nuovoStato, caricamento, TS)

import Eventi.Amministrazione
import Eventi.Anagrafe
import Eventi.Accredito
import Eventi.Impegno
import Eventi.Ordine

sel :: ((Persistenza QS, Sessione) -> IO a) -> Interfaccia a
sel f = asks f >>= liftIO 

-- | la monade dove gira il programma. Mantiene in lettura lo stato del gruppo insieme alle operazioni di IO. Nello stato la lista degli eventi aspiranti un posto nella patch
type MEnv = ReaderT (Persistenza QS, Sessione) IO
type Interfaccia = Costruzione MEnv () 

applicazione :: Costruzione MEnv () ()
applicazione = rotonda $ \_ -> do 
	ms <- sel $ readStato . fst 
	case ms of 
		Nothing ->    
			mano "il gruppo non esiste ancora" 
				[("creazione nuove chiavi di responsable", bootChiavi)
				,("creazione nuovo stato di gruppo", bootGruppo [])
				]
		Just s -> do 
			mk <- accesso 
			case mk of 
				Nothing -> mano "accesso anonimo" [("interrogazioni", interrogazioni (fst s))
						,("creazione nuove chiavi di responsable", bootChiavi)
						]
				Just q@((u,_),_) -> do
					mano ("accesso secondo " ++ u) [
						("eventi" , mano "produzione eventi" $ 
							[("votazioni",votazioni q)
							,("economia",economia q)
							,("anagrafe",anagrafica q)
							,("correzione",eliminazioneEvento q)
							]),
						("interrogazione", caricando q >>= interrogazioni),
						("amministrazione",amministrazione q)
						]
					salvaPatch q

-- | comunica che c'è un errore logico nella richiesta
bocciato :: String -> Interfaccia ()
bocciato x =  P.errore . Response $ [("Incoerenza", ResponseOne x)] 

-- | semplifica il running dei Supporto
conStato :: (String -> Interfaccia a) -> (b -> Interfaccia a) -> Supporto MEnv TS () b -> TS -> Interfaccia a
conStato x y z s = runSupporto s x y z

type Accesso = (Responsabile, Supporto MEnv TS () (Firmante TS))
-- | prova a fare accedere un responsabile. ritorna Nothing segnalando accesso anonimo
accesso :: Interfaccia  (Maybe Accesso)
accesso = rotonda $ \k -> do
	(s,_) <- letturaStato 
	conStato bocciato k login s

bootGruppo ::  [Responsabile] -> Interfaccia ()
bootGruppo xs = do
	P.output . ResponseMany $ map fst xs
	mano "inserimento responsabili iniziali" $ 
		[("caricamento altra chiave responsabile", P.upload "chiave" >>= \x -> bootGruppo (x:xs))
		,("fine caricamento chiavi", sel $ ($nuovoStato xs) . writeStato . fst)
		]

bootChiavi :: Interfaccia ()
bootChiavi = do
	u <- P.libero "scegli il tuo nome di utente"
	p <- P.libero "immetti una password, una frase , lunga almeno 12 caratteri"
	P.download (u ++ ".chiavi") (u,cryptobox p)

letturaStato ::  Interfaccia QS
letturaStato = fmap fromJust . sel $ readStato . fst

interrogazioni :: TS -> Interfaccia ()
interrogazioni s =  do
	liftIO $ print s
	mano "interrogazione stato del gruppo" . concatMap ($ bocciato) $ [
		costrQueryAccredito s P.output,
		costrQueryAnagrafe s P.output,
		costrQueryOrdine s P.output,
		costrQueryAssenso s P.output,
		costrQueryImpegni s P.output
		]

letturaEventi ::  Interfaccia [Evento]
letturaEventi = sel $ readEventi . snd

svuotaEventi :: Interfaccia ()
svuotaEventi = sel $ ($[]). writeEventi . snd

salvaPatch  :: Accesso -> Interfaccia ()
salvaPatch q@((u,_),sf) = do
	(s,_) <- letturaStato
	evs <- letturaEventi
	let salvataggio = do
		let p up = sel $ ($up) . ($u) . writeUPatch . fst
		runSupporto s bocciato (\(Firmante f) -> p (f s evs) >> svuotaEventi) sf
	when (not $ null evs) . mano "trattamento eventi in sospeso"  $
		[("firma",salvataggio),("mantieni",return ()),("elimina", svuotaEventi), ("scarica eventi", P.download "eventi.txt" evs)]

caricando :: Accesso -> Interfaccia TS
caricando q@((u,_),_) = do 
	evs <- letturaEventi 
	s <- letturaStato
	liftIO $ print evs	
	let (s',logs) =  caricamento (map ((,) u) evs) $ s
	liftIO $ print logs
	return $ fst s'
	
eliminazioneEvento :: Accesso -> Interfaccia ()
eliminazioneEvento ((u,_),_) = do
	es <- letturaEventi
	if null es then bocciato "non ci sono eventi da eliminare"
		else do 
			x <- P.scelte (zip es es) "seleziona evento da eliminare"
			correzioneEventi $ delete x es


-- correzioneEventi  :: Show a => a -> Interfaccia ()
correzioneEventi evs  = sel $ ($evs) . writeEventi . snd 

addEvento x = letturaEventi >>= \evs -> correzioneEventi (show x:evs)

anagrafica :: Accesso -> Interfaccia ()
anagrafica q@((u,_),_) = caricando q >>= \s -> do
	mano "anagrafe" . concatMap ($ bocciato) $ [
		costrEventiResponsabili s (addEvento),
		costrEventiAnagrafe s (addEvento)
		]


economia  :: Accesso -> Interfaccia () 
economia q@((u,_),_) = caricando q >>= \s -> do
	mano "economia" . concatMap ($ bocciato) $ [
		costrEventiAccredito s (addEvento),
		costrEventiImpegno s (addEvento) ,
		costrEventiOrdine s (addEvento)
		]

votazioni :: Accesso -> Interfaccia ()
votazioni q@((u,_),_) = caricando q >>= \s -> do
	n <- conStato (const $ return 0) (return . length) (assensiFiltrati u) s
	mano ("votazioni (" ++ show n ++ " votazioni in attesa)") . concatMap ($ bocciato) $ [
		costrEventiAssenso u s (addEvento)
		]

-- amministrazione :: (Responsabile,Firmante) -> Interfaccia ()
amministrazione q@(_,sf) = do
	let	sincronizza = do
			let p g = sel $ ($g). writeGPatch .fst
			rs <- sel $ readUPatches .fst
			(s,_) <- letturaStato
			case rs of 
				[] -> bocciato $ "nessun aggiornamento individale per lo stato attuale"
				xs -> runSupporto s bocciato (\(Firmante f) -> p $ f s xs) sf
	mano "amministrazione" $ [
			("firma un aggiornamento di gruppo (pericoloso)", sincronizza)
			]


