
{-# LANGUAGE FlexibleContexts, Rank2Types, ExistentialQuantification, ScopedTypeVariables, GeneralizedNewtypeDeriving, NoMonomorphismRestriction, ImplicitParams #-}
module Core.UI where

import Data.Maybe (isJust , fromJust)
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
import Core.Patch (login, Firmante (..),firmante)
import Core.Costruzione (runSupporto, Supporto)
import Core.Persistenza (Persistenza (..))
import Core.Sessione (Sessione (..))
import Core.Applicazione (QS,caricamento, TS)

import Eventi.Anagrafe
import Eventi.Accredito
import Eventi.Impegno
import Eventi.Ordine
-- sel :: (MonadReader (Persistenza QS, Sessione)  m, MonadIO m) => ((Persistenza QS, Sessione) -> IO b) -> m b
sel f = asks f >>= liftIO 

letturaStato' :: (Functor m, MonadReader (Persistenza QS, Sessione) m,MonadIO m) => m QS
letturaStato' = fmap fromJust . sel $ readStato . fst

caricando :: (Persistenza QS, Sessione) -> (Persistenza QS, Sessione)
caricando (pe,se) = 
	let 	modifica _ Nothing = return Nothing
		modifica se (Just s) = do  
			evs <- readEventi se
			mr <- readAccesso se
			case mr of 
				Nothing -> return $ Just s
				Just (u,_) -> do
					let (s',logs) =  caricamento (map ((,) u) evs) $ s
					print logs
					return . Just $ s'
	in (pe{readStato = readStato pe >>= modifica se},se) 

letturaStato = local caricando $ letturaStato' 
-- | semplifica il running dei Supporto

conStato :: (String -> Interfaccia a) -> (b -> Interfaccia a) -> Supporto MEnv TS () b ->  Interfaccia a
conStato x y z = runSupporto (fst <$> letturaStato) x y z

-- | comunica che c'è un errore logico nella richiesta
bocciato :: String -> Interfaccia ()
bocciato x =  P.errore . Response $ [("Incoerenza", ResponseOne x)] 

accesso :: Interfaccia (Maybe Responsabile)
accesso = do 
	ac <- rotonda $ \k -> conStato bocciato k login 
	sel $ ($ac) . writeAccesso . snd
	return ac
onAccesso k = do
	mr <- sel $ readAccesso . snd
	maybe (accesso >>= maybe (return ()) k) k mr

-- | la monade dove gira il programma. Mantiene in lettura lo stato del gruppo insieme alle operazioni di IO. Nello stato la lista degli eventi aspiranti un posto nella patch
type MEnv  = ReaderT (Persistenza QS, Sessione) IO 

type Interfaccia a = Costruzione MEnv () a

bootGruppo :: Interfaccia ()
bootGruppo = rotonda $ \k ->  
		mano "preparazione stato iniziale" $ 
			[("elenco chiavi responsabile già inserite", do
				xs <- sel $ readBoot .fst 
				P.output $ Response 
					[("elenco chiavi responsabile già inserite",
						ResponseMany $ map (ResponseOne . fst) xs)])
			,("inserimento di una chiave responsabile", P.upload "chiave" >>= \x -> do
				xs <- sel $ readBoot . fst
				sel $ ($ x: delete x xs) . writeBoot . fst)
			,("creazione dello stato iniziale dalle chiavi inserite", 
				sel (writeStato . fst) >> k ())
			]
	
bootChiavi :: Interfaccia ()
bootChiavi = do
	u <- P.libero "scegli il tuo nome di utente"
	p <- P.libero "immetti una password, una frase , lunga almeno 12 caratteri"
	P.download (u ++ ".chiavi") (u,cryptobox p)

wrapCostrActions 	
	:: (a -> Interfaccia ()) 
	-> [MEnv TS -> (a -> Interfaccia ()) -> (String -> Interfaccia ()) -> [(String,Interfaccia ())]]
	-> [(String,Interfaccia ())]
wrapCostrActions g = concatMap (\f -> f (fst <$> letturaStato) g bocciato)

interrogazioni :: Interfaccia ()
interrogazioni = mano "interrogazione stato del gruppo" . wrapCostrActions P.output $ [
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


salvataggio = onAccesso $ \(r@(u,_)) -> do
		let 	p up = sel $ ($up) . ($u) . writeUPatch . fst
		 	k (Firmante f) = do 
				evs <- letturaEventi
				(fst <$> letturaStato') >>= \s -> p (f s evs) >> svuotaEventi
		runSupporto (fst <$> letturaStato') bocciato k $ firmante r

correzioneEventi  :: [Evento] -> Interfaccia ()
correzioneEventi evs  = sel $ ($evs) . writeEventi . snd 


eliminazioneEvento :: Interfaccia ()
eliminazioneEvento = do
	es <- letturaEventi
	if null es then bocciato "non ci sono eventi da eliminare"
		else do 
			x <- P.scelte (zip es es) "seleziona evento da eliminare"
			correzioneEventi $ delete x es




addEvento x = letturaEventi >>= \evs -> correzioneEventi (show x:evs)

anagrafica :: Interfaccia ()
anagrafica = mano "anagrafe" . wrapCostrActions addEvento $ [
		costrEventiResponsabili,
		costrEventiAnagrafe 
		]


economia  :: Interfaccia () 
economia = mano "economia" . concat $ 
		[wrapCostrActions addEvento [costrEventiAccredito]
		,wrapCostrActions addEvento [costrEventiImpegno]
		,wrapCostrActions addEvento [costrEventiOrdine]
		]

votazioni :: Interfaccia ()
votazioni = onAccesso $ \(u,_) -> do
	
	n <- conStato (const $ return 0) (return . length) (assensiFiltrati u) 
	mano ("votazioni (" ++ show n ++ " votazioni in attesa)") . wrapCostrActions addEvento $ [
		costrEventiAssenso u
		]


amministrazione :: Interfaccia ()
amministrazione = do
	
	let	aggiornamenti = sel $ readUPatches .fst
		aggiornamento g = sel $ ($g). writeGPatch .fst
		sincronizza  = onAccesso $ \(r@(u,_)) -> do  
			rs <- aggiornamenti
			case rs of 
				[] -> bocciato $ "nessun aggiornamento individale per lo stato attuale"
				xs -> do
					let k (Firmante f)  = do
						(s,_) <- letturaStato
						aggiornamento $ f s xs
					runSupporto (fst <$> letturaStato) bocciato k $ firmante r

	mano "amministrazione" $ [
			("creazione nuove chiavi di responsable", bootChiavi),
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
				,("preparazione stato iniziale di gruppo", bootGruppo)
				]
		Just s ->  
			mano ("menu principale") [
				("accesso", mano "accesso" $ [("scelta responsabile", do  
					r <- accesso 
					when (isJust r) $ 
						P.output $ Response [("responsabile scelto" , 
							ResponseOne . fst . fromJust $ r)]
					),("firma aggiornamento",salvataggio)]),
				("eventi" , mano "produzione eventi" $ 
					[("votazioni",votazioni)
					,("economia",economia)
					,("anagrafe",anagrafica)
					,("correzione",eliminazioneEvento)
					,("scarica eventi", letturaEventi >>= P.download "eventi.txt" )
					]),
				("interrogazione", interrogazioni),
				("amministrazione",amministrazione)
				]

{-
-}
