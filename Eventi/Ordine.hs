{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, ViewPatterns, NoMonomorphismRestriction #-}
module Eventi.Ordine {-
	(statoInizialeOrdini, reazioneOrdine, StatoOrdini, makeAperturaOrdine,priorityOrdine,queryOrdine) -}
	where

import Data.List (isPrefixOf, tails)
import Control.Arrow 
import Data.Maybe
import Control.Monad.Reader (asks,ask, when, MonadReader)
import Control.Monad.Error (throwError)
import Text.PrettyPrint
import Debug.Trace

import Lib.Aspetti ((.<), ParteDi,see)
import Lib.Prioriti (R(..))
import Lib.Assocs (update,elimina,assente)
import Lib.Response (Response (..))

import Core.Costruzione (libero, scelte , CostrAction, runSupporto)
import Core.Parsing (Parser)
import Core.Programmazione (Effetti, Reazione (..) , EventoInterno (..), soloEsterna, nessunEffetto)
import Core.Inserimento (MTInserzione, conFallimento, fallimento, osserva, modifica, logga)

import Eventi.Anagrafe (Utente,validante,programmazioneAssenso,maggioranza)
import Eventi.Accredito (salda)
import Eventi.Impegno (programmazioneImpegno)

type Indice = Int
data EsternoOrdine = AperturaOrdine String deriving (Read,Show) 
priorityOrdine = R k  where
	k (AperturaOrdine _) = -28 

data StatoOrdini = StatoOrdini {chiusi :: [(String, Maybe (Utente, [(Utente,Float)]))] , aperti :: [(Indice,String)], inapertura :: [(Indice,String)]}
	deriving (Read,Show)

type TyOrdini a = (StatoOrdini , a)

bootOrdini :: a -> TyOrdini a
bootOrdini x = (StatoOrdini [] [] [], x)

reazioneOrdine = soloEsterna reattoreOrdine where
	
	reattoreOrdine (first validante -> (w, AperturaOrdine b)) = w $ \r -> do
		
		s@(StatoOrdini cs as ias ) <-  osserva
		fallimento (not (b `assente` cs) || (b `elem` map snd as) || (b `elem` map snd ias)) "nome non più disponibile"
		let 	positivo i = do 	
				(l,z) <- programmazioneImpegno ("l'acquisto " ++ b) r
				let t k = case k of
					Just us -> do 
						salda r (subtract . sum . map snd $ us)
						modifica $ \(StatoOrdini  cs as ias) -> StatoOrdini ((b,Just (r,us)):cs) (elimina l as) ias
						logga $ "acquisto " ++ b ++ " chiuso con successo"
						return nessunEffetto
					Nothing -> do
						modifica $ \(StatoOrdini  cs as ias ) -> StatoOrdini ((b,Nothing):cs) (elimina l as) ias
						logga $ "acquisto  " ++ b ++ " chiuso negativamente"
						return nessunEffetto
				modifica $ \(StatoOrdini cs as ias) -> StatoOrdini cs ((l,b):as) (elimina i ias)
				logga $  "aperto l'acquisto " ++ b 
				return ([z t],[])
			negativo i = do		modifica $ \(StatoOrdini cs as ias) -> StatoOrdini cs as (elimina i ias)
						logga $ "nuova proposta di acquisto " ++ b ++ " fallita"
						return nessunEffetto
		(i,z) <- programmazioneAssenso ("nuova proposta di acquisto " ++ b) r maggioranza positivo negativo	
		modifica $ \(StatoOrdini cs as ias) -> StatoOrdini cs as ((i,b):ias) 
		return (True,([z],[]))

costrEventiOrdine :: (Monad m, StatoOrdini `ParteDi` s) => CostrAction m c EsternoOrdine s
costrEventiOrdine s kp kn  = [("nuova proposta di acquisto", eventoApertura)] 
	where
	eventoApertura  = runSupporto s kn kp $ do
		n <- libero "nome della nuova proposta d'acquisto"
		StatoOrdini xs ys zs <- asks see
		when (n `elem` (map fst xs ++ map snd ys ++ map snd zs)) $ throwError "nome non più disponibile"
		return $ AperturaOrdine n

sottostringa :: Eq a => [a] -> [a] -> Bool
sottostringa x = any (x `isPrefixOf`) . tails

costrQueryOrdine :: (Monad m, ParteDi StatoOrdini s) => CostrAction m c Response s
costrQueryOrdine s kp kn = 	[("acquisti chiusi",cerca)]
	where
	run = runSupporto s kn kp
	cerca = run $ do
		(StatoOrdini xs _ _) <- asks see
		t <- libero "introduci parte del nome dell'acquisto [* per vederli tutti]"
		let cs =  filter (sottostringa (if t == "*" then "" else t) . fst) $ xs 
		when (null cs) . throwError $ "nessun nome di acquisto incontra la richiesta"
		r <- scelte cs "acquisto da esaminare" 
		return $ case r of 
			Nothing -> ResponseOne "acquisto fallito" 
			Just (autore, xs) -> Response [("responsabile dell'acquisto",ResponseOne autore), 
						("acquirenti",ResponseAL  xs)]
		

