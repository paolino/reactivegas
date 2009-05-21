{-# LANGUAGE ScopedTypeVariables, ViewPatterns, NoMonomorphismRestriction #-}
module Ordine (statoInizialeOrdini, reazioneOrdine, StatoOrdini, makeAperturaOrdine,priorityOrdine) where

import Lib1
import Lib0
import Impegno (programmazioneImpegno)
import Accredito (salda)
import Anagrafe (Utente,eventoValidato)
import Aspetti ((.<))
import Core (nessunEffetto, soloEsterna)
import Costruzione
import Prioriti

data EventoOrdine = AperturaOrdine String deriving (Read,Show) 
priorityOrdine = R k  where
	k (AperturaOrdine _) = -20 

data StatoOrdini = StatoOrdini {chiusi :: [(String, Maybe (Utente, [(Utente,Float)]))] , aperti :: [(Indice,String)]}
	deriving (Read,Show)

statoInizialeOrdini x = StatoOrdini [] [] .< x

reazioneOrdine = soloEsterna reattoreOrdine where
	reattoreOrdine (eventoValidato -> (w, AperturaOrdine b)) = w $ \r -> do
		s@(StatoOrdini cs as) <- osserva
		fallimento (not (b `assente` cs) || (b `elem` map snd as)) "il nome per questo bene e' gia' stato utilizzato"
		(l,z) <- programmazioneImpegno ("impegno di acquisto del bene " ++ b) r
		let t k = case k of
			Just us -> do 
				salda r (subtract . sum . map snd $ us)
				modifica $ \(StatoOrdini  cs as) -> StatoOrdini ((b,Just (r,us)):cs) (elimina l as)
				logga $ "ordine per il bene " ++ show b ++ " chiuso con successo"
				return nessunEffetto
			Nothing -> do
				modifica $ \(StatoOrdini  cs as) -> StatoOrdini ((b,Nothing):cs) (elimina l as)
				logga $ "ordine per il bene " ++ show b ++ " fallito"
				return nessunEffetto
		modifica $ \(StatoOrdini cs as) -> StatoOrdini cs ((l,b):as)
		logga $  "per il bene " ++ show b ++ " aperto l'ordine numero " ++ show l
		return (True,([z t],[]))

makeAperturaOrdine k = [("evento di apertura ordini per un nuovo bene", eventoApertura)] where
	eventoApertura = do
		n <- parametro (Libero "nome del nuovo bene in acquisto")
		return $ show (AperturaOrdine n)

