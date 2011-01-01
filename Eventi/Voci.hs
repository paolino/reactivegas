

{-# LANGUAGE NoMonomorphismRestriction, FlexibleInstances, FlexibleContexts, Rank2Types, ScopedTypeVariables, GADTs, ExistentialQuantification, StandaloneDeriving,UndecidableInstances #-}
module Eventi.Voci where

import Data.List (nub)
import Control.Arrow ((&&&))
import Control.Monad.Cont (callCC)
import Lib.Units -- (Pesi,Volumi,Unità, Denaro,UnitClass (..))
import Lib.NaturalLanguage
import Lib.QInteger
import Lib.Passo
import Eventi.Servizio
import Core.Costruzione (CostrAction)
import Lib.Response

import Voci.Core
import Voci.Instances
import Voci.UI.Commercio

type Indice = QInteger


data EsternoVoci = NuovaVoce Voce | EliminaVoce Indice deriving (Show,Read)



-- | costruttore di eventi per il modulo di accredito
costrEventiVoci :: (Functor m, Monad m) => CostrAction m c EsternoVoci s
costrEventiVoci s kp kn = 	[("nuova voce (sperimentale)", callCC (nuovaVoce [] [] Nothing) >>= kp) 
				] 
	where
	nuovaVoce cs fs co k = do
		let c= Response [("categorie", ResponseMany $ map ResponseOne $ cs),
			("filiera", ResponseMany $  map ResponseOne $ fs),
			("unità minima di acquisto", ResponseOne $ maybe "nessuno" (render . singolare) co)]
		menu c $ [
			("aggiungi una categoria", addCat),
			("elimina una categoria", rmCat),
			("aggiungi un attore nella filiera", addFil),
			("elimina un attore dalla filiera", rmFil),
			("imposta l'unità minima di acquisto",setCom),
			("<fine>", case co of 
				Nothing -> do 
					errore True $ ResponseOne "manca la definizione diunità minima di acquisto"
					nuovaVoce cs fs co k
				Just o -> return $ NuovaVoce $ Voce cs fs o)
			]
		where
			addCat = libero (ResponseOne "nuova categoria") >>= \c -> nuovaVoce (nub (c:cs)) fs co k
			addFil = libero (ResponseOne "nuovo attore della filiera") >>= \c -> nuovaVoce cs (nub (c:fs)) co k
			rmCat = scelte (map (id &&& id) cs)  (ResponseOne "selezione categoria da eliminare") >>= \c -> 
				nuovaVoce (filter (/=c) cs) fs co k
			rmFil = scelte (map (id &&& id) fs) (ResponseOne "selezione attore da eliminare") >>= \c -> 
				nuovaVoce cs (filter (/=c) fs) co k
			setCom = ui >>= \o -> nuovaVoce cs fs (Just o) k



