
{-# LANGUAGE FlexibleContexts, ExistentialQuantification, ScopedTypeVariables, GeneralizedNewtypeDeriving, NoMonomorphismRestriction, ImplicitParams #-}
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



-- | la monade dove gira il programma. Mantiene in lettura lo stato del gruppo insieme alle operazioni di IO. Nello stato la lista degli eventi aspiranti un posto nella patch
type MEnv = ReaderT (Persistenza QS, Sessione) IO
type Interfaccia = Costruzione IO () 

applicazione :: (Persistenza QS, Sessione) -> Interfaccia ()
applicazione ctx = let ?context = ctx in rotonda $ \k -> do
	ms <- liftIO $ ($ (?context)) (readStato . fst) 
	case ms of 
		Nothing ->    
			mano "il gruppo non esiste ancora" 
				[("creazione nuove chiavi di responsable", bootChiavi)
				,("creazione nuovo stato di gruppo", bootGruppo [])
				]
		Just s -> do 
			mk <- accesso 
			case mk of 
				Nothing -> mano "accesso anonimo" [("interrogazioni", interrogazioni)
						,("creazione nuove chiavi di responsable", bootChiavi)
						]
				Just q@((u,_),_) -> 
					mano ("accesso secondo " ++ u) [
						("eventi" , mano "produzione eventi" $ 
							[("votazioni",votazioni q)
							,("economia",economia q)
							,("anagrafe",anagrafica q)
							,("correzione",eliminazioneEvento q)
							]),
						("interrogazione", caricando q interrogazioni),
						("amministrazione",amministrazione q)
						]


-- | comunica che c'è un errore logico nella richiesta
bocciato :: String -> Interfaccia ()
bocciato x =  P.errore . Response $ [("Incoerenza", ResponseOne x)] 

-- | semplifica il running dei Supporto
conStato :: (?context::(Persistenza QS, Sessione)) => (String -> Interfaccia a) -> (b -> Interfaccia a) -> Supporto IO TS () b -> Interfaccia a
conStato x y z = do	(s,_) <- letturaStato
			runSupporto s x y z

type Accesso = (Responsabile, Supporto IO TS () (Firmante TS))
-- | prova a fare accedere un responsabile. ritorna Nothing segnalando accesso anonimo
accesso ::(?context::(Persistenza QS, Sessione)) => Interfaccia  (Maybe Accesso)
accesso = rotonda $ \k -> conStato bocciato k login 

bootGruppo :: (?context::(Persistenza QS, Sessione)) => [Responsabile] -> Interfaccia ()
bootGruppo xs = do
	P.output . ResponseMany $ map fst xs
	mano "inserimento responsabili iniziali" $ 
		[("caricamento altra chiave responsabile", P.upload "chiave" >>= \x -> bootGruppo (x:xs))
		,("fine caricamento chiavi", liftIO $ (writeStato . fst $ (?context)) $ nuovoStato xs)
		]
	liftIO $ print "ola"

bootChiavi :: Interfaccia ()
bootChiavi = do
	u <- P.libero "scegli il tuo nome di utente"
	p <- P.libero "immetti una password, una frase , lunga almeno 12 caratteri"
	P.download (u ++ ".chiavi") (u,cryptobox p)

letturaStato :: (?context::(Persistenza QS, b)) => Interfaccia QS
letturaStato = fmap fromJust . liftIO $ ($ (?context)) (readStato . fst) 

interrogazioni :: (?context::(Persistenza QS, Sessione)) => Interfaccia ()
interrogazioni =  do
	(s,_) <- letturaStato
	mano "interrogazione stato del gruppo" . concatMap ($ bocciato) $ [
		costrQueryAccredito s P.output,
		costrQueryAnagrafe s P.output,
		costrQueryOrdine s P.output,
		costrQueryAssenso s P.output,
		costrQueryImpegni s P.output
		]

letturaEventi :: (?context::(Persistenza QS, Sessione)) => Interfaccia [Evento]
letturaEventi = liftIO $ ($ (?context)) (readEventi . snd) 

svuotaEventi :: (?context::(Persistenza QS, Sessione)) => Interfaccia ()
svuotaEventi = liftIO $  (writeEventi . snd $ (?context)) []

salvaPatch  ::(?context::(Persistenza QS, Sessione)) => Accesso -> Interfaccia ()
salvaPatch q@((u,_),sf) = do
	(s,_) <- letturaStato
	evs <- letturaEventi
	let salvataggio = do
		let p = ($ (?context))  (writeUPatch . fst)
		runSupporto s bocciato (\(Firmante f) -> liftIO (p u $ f s evs)) sf
	when (not $ null evs) . mano "trattamento eventi in sospeso"  $
		[("firma",salvataggio),("mantieni",return ()),("elimina", svuotaEventi), ("scarica eventi", P.download "eventi.txt" evs)]

caricando ::(?context::(Persistenza QS, Sessione)) => Accesso -> Interfaccia a -> Interfaccia a
caricando q@((u,_),_) k = do 
	evs <- letturaEventi  
	s <- letturaStato
	if null evs then k  else  do 
		let c = Just . fst . caricamento (map ((,) u) evs) . fromJust  --buggy, dove sono i logs ?
		let ?context = (first $ \f -> f {readStato = c <$> readStato f}) (?context) in k 

eliminazioneEvento ::(?context::(Persistenza QS, Sessione)) => Accesso -> Interfaccia ()
eliminazioneEvento ((u,_),_) = do
	es <- letturaEventi
	if null es then bocciato "non ci sono eventi da eliminare"
		else do 
			x <- P.scelte (zip es es) "seleziona evento da eliminare"
			correzioneEventi $ delete x es


-- correzioneEventi  :: Show a => a -> Interfaccia ()
correzioneEventi evs  = liftIO $ (writeEventi . snd $ (?context)) evs

addEvento x = letturaEventi >>= \evs -> correzioneEventi (show x:evs)

anagrafica ::(?context::(Persistenza QS, Sessione)) => Accesso -> Interfaccia ()
anagrafica q@((u,_),_) = caricando q $ do
	(s,_) <- letturaStato
	mano "anagrafe" . concatMap ($ bocciato) $ [
		costrEventiResponsabili s (addEvento),
		costrEventiAnagrafe s (addEvento)
		]


economia  ::(?context::(Persistenza QS, Sessione)) => Accesso -> Interfaccia () 
economia q@((u,_),_) = caricando q $ do
	(s,_) <- letturaStato
	mano "economia" . concatMap ($ bocciato) $ [
		costrEventiAccredito s (addEvento),
		costrEventiImpegno s (addEvento) ,
		costrEventiOrdine s (addEvento)
		]

votazioni ::(?context::(Persistenza QS, Sessione)) => Accesso -> Interfaccia ()
votazioni q@((u,_),_) = caricando q $ do
	n <- conStato (const $ return 0) (return . length) $ assensiFiltrati u
	(s,_) <- letturaStato
	mano ("votazioni (" ++ show n ++ " votazioni in attesa)") . concatMap ($ bocciato) $ [
		costrEventiAssenso u s (addEvento)
		]

-- amministrazione :: (Responsabile,Firmante) -> Interfaccia ()
amministrazione q@(_,sf) = do
	let	sincronizza = do
			let p = ($ (?context)) (writeGPatch .fst)
			rs <- liftIO $ ($ (?context)) (readUPatches .fst )
			(s,_) <- letturaStato
			case rs of 
				[] -> bocciato $ "nessun aggiornamento individale per lo stato attuale"
				xs -> runSupporto s bocciato (\(Firmante f) -> liftIO (p $ f s xs)) sf
	mano "amministrazione" $ [
			("firma un aggiornamento di gruppo (pericoloso)", sincronizza)
			]


