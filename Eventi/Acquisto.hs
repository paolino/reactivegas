{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, ViewPatterns, NoMonomorphismRestriction , DeriveDataTypeable#-}
module Eventi.Acquisto {-
	(statoInizialeAcquisti, reazioneAcquisto, StatoAcquisti, makeAperturaAcquisto,priorityAcquisto,queryAcquisto) -}
	where

import Data.Typeable (Typeable)
import Data.List (isPrefixOf, tails, find, deleteBy)
import Control.Arrow (first, (&&&))
import Data.Maybe
import Control.Applicative ((<$>))
import Control.Monad.Reader (asks,ask, when, MonadReader)
import Control.Monad.Error (throwError)
import Control.Monad.State (get)
import Debug.Trace

import Lib.ShowRead
import Lib.Aspetti ((.<), ParteDi,see)
import Lib.Prioriti (R(..))
import Lib.Assocs (update,elimina,assente)
import Lib.Response (Response (..))
import Lib.QInteger (QInteger)
import Lib.Euro (Euro)

import Core.Types (Utente)
import Core.Costruzione (libero, scelte , CostrAction, runSupporto)
import Core.Programmazione (Effetti, Reazione (..) , EventoInterno (..), soloEsterna, nessunEffetto, Message (..))
import Core.Inserimento (MTInserzione, conFallimento, fallimento, osserva, modifica, loggamus, logga)
import Core.Parsing (Parser, ParserConRead (ParserConRead))
import Core.Dichiarazioni (Dichiarazione(Singola), Singola)


import Eventi.Anagrafe (validante,programmazioneAssenso,maggioranza)
import Eventi.Impegno (programmazioneImpegno', raccolte, Impegni)
import Eventi.Servizio (Servizio)

type Indice = QInteger
data EsternoAcquisto = AperturaAcquisto String  
data InternoAcquisto = IAperturaAcquisto Indice String | IChiusuraAquisto Indice Bool deriving (Show,Read)

priorityAcquisto = R k  where
	k (AperturaAcquisto _) = -28 

-----------------  vestigia del passato ---------------------------------------------------
data StatoAcquisti = StatoAcquisti { chiusi :: [()], aperti :: [()]}  deriving (Read,Show,Eq) -- vestigia del passato

type TyAcquisti a = (StatoAcquisti , a)

bootAcquisti :: a -> TyAcquisti a
bootAcquisti x = (StatoAcquisti [] [], x)
--------------------------------------------------------------


instance Show EsternoAcquisto where
	show (AperturaAcquisto x) = "apertura nuovo acquisto di nome \"" ++ x ++ "\""

instance Read EsternoAcquisto where
	readPrec = lift $ do
		string "apertura nuovo acquisto di nome "
		AperturaAcquisto `fmap` phrase

data FineAcquisto = FineAcquisto String [(Utente,Euro)] deriving (Typeable, Show)

reazioneAcquisto = soloEsterna reattoreAcquisto where
	
	reattoreAcquisto (first validante -> (w, AperturaAcquisto b)) = w $ \r -> do
		rs <- raccolte <$> get
		fallimento (b `elem` rs) "nome non disponibile"
		let t k = case k of
			Just us -> do 
				logga . Message $ FineAcquisto b us
				return nessunEffetto
			_ -> return nessunEffetto
		
		(li,fi,zi,ci,ievs) <- programmazioneImpegno' b r t
		-- definizione completamenti raccolta di assenso
		let 	positivo _ = do
				ci
				loggamus $ "concessa la chiusura dell'acquisto " ++ b -- esegui la marcatura ottenuta da programmazione impegno
				return ([],[])
			negativo _ = do
				loggamus $ "negata la chiusura dell'acquisto, acquisto fallito " ++ b
				(epr,epf) <- fi
				return (epr,epf)
		(la,za,esf) <- programmazioneAssenso ("nuova proposta di acquisto " ++ b) r maggioranza  positivo negativo

		return (True, ([za]++ zi esf,ievs))

costrEventiAcquisto :: (Monad m, Parser p EsternoAcquisto,  Servizio Impegni `ParteDi` s) => CostrAction m c (Dichiarazione p Singola) s
costrEventiAcquisto s kp kn  = [("nuova proposta di acquisto", eventoApertura)] 
	where
	eventoApertura  = runSupporto s kn kp $ do
		n <- libero  $ ResponseOne  "nome della nuova proposta d'acquisto"
		rs <- asks raccolte
		when (n `elem` rs) $ throwError "acquisto già aperto"
		return . Singola  $ AperturaAcquisto n

{-
sottostringa :: Eq a => [a] -> [a] -> Bool
sottostringa x = any (x `isPrefixOf`) . tails

costrQueryAcquisto :: (Monad m, ParteDi StatoAcquisti s) => CostrAction m c Response s
costrQueryAcquisto s kp kn = 	[("acquisti chiusi",cerca)]
	where
	run = runSupporto s kn kp
	cerca = run $ do
		t <- libero  "introduci parte del nome dell'acquisto [* per vederli tutti]"
		(StatoAcquisti xs _ ) <- asks see
		let cs =  filter (sottostringa (if t == "*" then "" else t) . fst) $ xs 
		when ( null cs) . throwError $ "nessun nome di acquisto incontra la richiesta"
		r <- scelte  (map (fst &&& fst) cs) "acquisto da esaminare" 
		(StatoAcquisti xs _ ) <- asks see	
		case lookup r xs of
			Nothing -> throwError $ "non esiste l'acquisto " ++ r 
			Just r' -> return $ case r' of 
				Nothing -> ResponseOne "acquisto fallito" 
				Just (autore, xs) -> Response [("responsabile dell'acquisto",ResponseOne autore), 
							("acquirenti",ResponseAL  xs)]
		
-}
