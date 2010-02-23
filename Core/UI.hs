
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
sel :: (MonadReader (Persistenza QS, Sessione)  m, MonadIO m) => ((Persistenza QS, Sessione) -> IO b) -> m b
sel f = asks f >>= liftIO 

letturaStato :: (Functor m, MonadReader (Persistenza QS, Sessione) m,MonadIO m) => m QS
letturaStato = fmap fromJust . sel $ readStato . fst
-- | semplifica il running dei Supporto

conStato :: (String -> Interfaccia a) -> (b -> Interfaccia a) -> Supporto MEnv TS () b ->  Interfaccia a
conStato x y z = runSupporto (fst <$> letturaStato) x y z

-- | la monade dove gira il programma. Mantiene in lettura lo stato del gruppo insieme alle operazioni di IO. Nello stato la lista degli eventi aspiranti un posto nella patch
type MEnv = ReaderT (Persistenza QS, Sessione) IO
type Interfaccia = Costruzione MEnv () 

-- | comunica che c'è un errore logico nella richiesta
bocciato :: String -> Interfaccia ()
bocciato x =  P.errore . Response $ [("Incoerenza", ResponseOne x)] 

type Accesso = (Responsabile, Supporto MEnv TS () (Firmante TS))
-- | prova a fare accedere un responsabile. ritorna Nothing segnalando accesso anonimo
accesso :: Interfaccia  (Maybe Accesso)
accesso = rotonda $ \k -> conStato bocciato k login 

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

caricando :: Accesso -> Interfaccia () -> Interfaccia ()
caricando q@((u,_),_) k = do
	let modifica se (Just s) = do  
		evs <- readEventi se
		let (s',logs) =  caricamento (map ((,) u) evs) $ s
		print logs
		return . Just $ s'
	local (\(pe,se) -> (pe{readStato = readStato pe >>= modifica se},se)) k

wrapCostrActions 	
	:: Maybe Accesso
	-> (a -> Interfaccia ()) 
	-> [MEnv TS -> (a -> Interfaccia ()) -> (String -> Interfaccia ()) -> [(String,Interfaccia ())]]
	-> [(String,Interfaccia ())]
wrapCostrActions Nothing g = concatMap $ \f -> f (fst <$> letturaStato) g bocciato
wrapCostrActions (Just q) g = map (second $ caricando q) . concatMap (\f -> f (fst <$> letturaStato) g bocciato)

interrogazioni :: Maybe Accesso -> Interfaccia ()
interrogazioni mq = mano "interrogazione stato del gruppo" . wrapCostrActions mq P.output $ [
		costrQueryAccredito,
		costrQueryAnagrafe,
		costrQueryOrdine,
		costrQueryAssenso,
		costrQueryImpegni
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
		runSupporto (fst <$> letturaStato) bocciato (\(Firmante f) -> p (f s evs) >> svuotaEventi) sf
	when (not $ null evs) . mano "trattamento eventi in sospeso"  $
		[	("firma",salvataggio)
		,	("mantieni",return ())
		,	("elimina", svuotaEventi)
		, 	("scarica eventi", letturaEventi >>= P.download "eventi.txt" )
		]


correzioneEventi  :: [Evento] -> Interfaccia ()
correzioneEventi evs  = sel $ ($evs) . writeEventi . snd 


eliminazioneEvento :: Accesso -> Interfaccia ()
eliminazioneEvento ((u,_),_) = do
	es <- letturaEventi
	if null es then bocciato "non ci sono eventi da eliminare"
		else do 
			x <- P.scelte (zip es es) "seleziona evento da eliminare"
			correzioneEventi $ delete x es




addEvento x = letturaEventi >>= \evs -> trace "correct" (correzioneEventi (show x:evs))

anagrafica :: Accesso -> Interfaccia ()
anagrafica q@((u,_),_) = mano "anagrafe" . wrapCostrActions (Just q) addEvento $ [
		costrEventiResponsabili,
		costrEventiAnagrafe 
		]


economia  :: Accesso -> Interfaccia () 
economia q@((u,_),_) = caricando q . mano "economia" . concat $ 
				[wrapCostrActions (Just q) addEvento [costrEventiAccredito]
				,wrapCostrActions (Just q) addEvento [costrEventiImpegno]
				,wrapCostrActions (Just q) addEvento [costrEventiOrdine]
				]

votazioni :: Accesso -> Interfaccia ()
votazioni q@((u,_),_) = caricando q $  do
	n <- conStato (const $ return 0) (return . length) (assensiFiltrati u) 
	mano ("votazioni (" ++ show n ++ " votazioni in attesa)") . wrapCostrActions (Just q) addEvento $ [
		costrEventiAssenso u
		]

-- amministrazione :: (Responsabile,Firmante) -> Interfaccia ()
amministrazione q@(_,sf) = do
	
	let	aggiornamenti = sel $ readUPatches .fst
		aggiornamento g = sel $ ($g). writeGPatch .fst
		sincronizza  = do 
			(s,_) <- letturaStato
			rs <- aggiornamenti
			case rs of 
				[] -> bocciato $ "nessun aggiornamento individale per lo stato attuale"
				xs -> runSupporto (fst <$> letturaStato) bocciato (\(Firmante f) -> aggiornamento $ f s xs) sf
	mano "amministrazione" $ [
			("firma un aggiornamento di gruppo (pericoloso)", sincronizza),
			("scarica gli aggiornamenti individuali", aggiornamenti >>= P.download "aggiornamenti.txt"),
			("carica aggiornamento di gruppo",P.upload "aggiornamento di gruppo" >>= aggiornamento), 
			("manutenzione", menu "manutenzione" $ [
				("scarica stato", sel (readStato . fst) >>= P.download "stato")
				])
			]

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
				Nothing -> mano "accesso anonimo" [("interrogazioni", interrogazioni Nothing)
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
						("interrogazione", interrogazioni (Just q)),
						("amministrazione",amministrazione q)
						]
					salvaPatch q


{-
-}
