{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, ViewPatterns, NoMonomorphismRestriction #-}
module Eventi.Acquisto {-
	(statoInizialeAcquisti, reazioneAcquisto, StatoAcquisti, makeAperturaAcquisto,priorityAcquisto,queryAcquisto) -}
	where

import Data.List (isPrefixOf, tails, find, deleteBy)
import Control.Arrow 
import Data.Maybe
import Control.Applicative ((<$>))
import Control.Monad.Reader (asks,ask, when, MonadReader)
import Control.Monad.Error (throwError)
import Text.PrettyPrint
import Debug.Trace

import Lib.Aspetti ((.<), ParteDi,see)
import Lib.Prioriti (R(..))
import Lib.Assocs (update,elimina,assente)
import Lib.Response (Response (..))
import Lib.QInteger (QInteger)

import Core.Costruzione (libero, scelte , CostrAction, runSupporto)
import Core.Parsing (Parser)
import Core.Programmazione (Effetti, Reazione (..) , EventoInterno (..), soloEsterna, nessunEffetto)
import Core.Inserimento (MTInserzione, conFallimento, fallimento, osserva, modifica, logga)

import Eventi.Anagrafe (Utente,validante,programmazioneAssenso,maggioranza)
import Eventi.Accredito (salda)
import Eventi.Impegno (programmazioneImpegno')

type Indice = QInteger
data EsternoAcquisto = AperturaAcquisto String deriving (Read,Show) 
priorityAcquisto = R k  where
	k (AperturaAcquisto _) = -28 

data Acquisto = Acquisto
	{nome		:: String 	-- ^ identificativo unico dell'acquisto 
	,concesso 	:: Bool		-- ^ stato della raccolta assensi
	,impegni	:: Indice	-- ^ indice della raccolta impegni
	,assensi	:: Indice	-- ^ indice della raccolta assensi
	} deriving (Show, Read)

nominato :: String -> Acquisto -> Bool
nominato s = (==) s . nome

concedi s = concessione <$> acquisto s	 
	where concessione (Acquisto n c i a) = Acquisto n True i a

acquisto b = do 
	(as :: [Acquisto]) <- aperti <$> osserva
	maybe (fallimento True "nome dell'acquisto inesistente" >> return undefined) return $ find (nominato b) as

data StatoAcquisti = StatoAcquisti 
	{ chiusi :: [(String, Maybe (Utente, [(Utente,Float)]))] 
	, aperti :: [Acquisto]
	} deriving (Read,Show)

type TyAcquisti a = (StatoAcquisti , a)

bootAcquisti :: a -> TyAcquisti a
bootAcquisti x = (StatoAcquisti [] [], x)

reazioneAcquisto = soloEsterna reattoreAcquisto where
	
	reattoreAcquisto (first validante -> (w, AperturaAcquisto b)) = w $ \r -> do
		-- reazione al nome inaccettabile	
		s@(StatoAcquisti cs as) <-  osserva
		fallimento (not (b `assente` cs) || (b `elem` map nome as)) "nome non più disponibile"
		-- definizione della chiusura raccolta impegno
		let compl x = modifica $ \(StatoAcquisti cs as) 
			-> StatoAcquisti ((b,x):cs) (filter (not . nominato b) as)
		let t k = do
			case k of
				Just us -> do 
					-- aggiorna la cassa del responsabile
					salda r (subtract . sum . map snd $ us)
					compl (Just (r,us))
					logga $ "acquisto " ++ b ++ " chiuso con successo"
					return nessunEffetto
				Nothing -> do
					compl Nothing
					logga $ "acquisto  " ++ b ++ " chiuso negativamente"
					return nessunEffetto
		(li,fi,zi) <- programmazioneImpegno' ("l'acquisto " ++ b) r t (concesso <$> acquisto b)
		-- definizione completamenti raccolta di assenso
		let 	positivo _ = do
				a <- concedi b
				modifica $ \(StatoAcquisti cs as)  -> StatoAcquisti cs (a: filter (not . nominato b) as)
				logga $ "concessa la chiusura dell'acquisto " ++ b
				return nessunEffetto
			negativo _ = do
				logga $ "negata la chiusura dell'acquisto, acquisto fallito " ++ b
				fi
		(la,za) <- programmazioneAssenso ("nuova proposta di acquisto " ++ b) r maggioranza  positivo negativo

		modifica $ \(StatoAcquisti cs as) -> StatoAcquisti cs (Acquisto b False li la : as)
		return (True, ([za, zi],[]))

costrEventiAcquisto :: (Monad m, StatoAcquisti `ParteDi` s) => CostrAction m c EsternoAcquisto s
costrEventiAcquisto s kp kn  = [("nuova proposta di acquisto", eventoApertura)] 
	where
	eventoApertura  = runSupporto s kn kp $ do
		n <- libero "nome della nuova proposta d'acquisto"
		StatoAcquisti xs ys <- asks see
		when (n `elem` (map fst xs ++ map nome ys)) $ throwError "nome non più disponibile"
		return $ AperturaAcquisto n

sottostringa :: Eq a => [a] -> [a] -> Bool
sottostringa x = any (x `isPrefixOf`) . tails

costrQueryAcquisto :: (Monad m, ParteDi StatoAcquisti s) => CostrAction m c Response s
costrQueryAcquisto s kp kn = 	[("acquisti chiusi",cerca)]
	where
	run = runSupporto s kn kp
	cerca = run $ do
		t <- libero "introduci parte del nome dell'acquisto [* per vederli tutti]"
		(StatoAcquisti xs _ ) <- asks see
		let cs =  filter (sottostringa (if t == "*" then "" else t) . fst) $ xs 
		when ( null cs) . throwError $ "nessun nome di acquisto incontra la richiesta"
		r <- scelte (map (fst &&& fst) cs) "acquisto da esaminare" 
		(StatoAcquisti xs _ ) <- asks see	
		case lookup r xs of
			Nothing -> throwError $ "non esiste l'acquisto " ++ r 
			Just r' -> return $ case r' of 
				Nothing -> ResponseOne "acquisto fallito" 
				Just (autore, xs) -> Response [("responsabile dell'acquisto",ResponseOne autore), 
							("acquirenti",ResponseAL  xs)]
		

