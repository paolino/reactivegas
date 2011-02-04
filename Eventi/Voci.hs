

{-# LANGUAGE NoMonomorphismRestriction, FlexibleInstances, FlexibleContexts, Rank2Types, ScopedTypeVariables, GADTs, ExistentialQuantification, StandaloneDeriving,UndecidableInstances, ViewPatterns #-}
module Eventi.Voci where

import Data.List (nub, intercalate)
import Control.Arrow ((&&&), first)
import Control.Monad.Cont (callCC, join, lift)
import Control.Monad.Reader (asks)
import Control.Monad.Error (throwError)
import Lib.Units -- (Pesi,Volumi,Unità, Denaro,UnitClass (..))
import Lib.NaturalLanguage
import Lib.QInteger
import qualified Lib.Passo as P
import Eventi.Servizio
import Core.Costruzione -- (CostrAction)
import Lib.Response
import Core.Types (Utente)
import Core.Programmazione (Reazione, soloEsterna, nessunEffetto, Message (..))
import Core.Inserimento (MTInserzione, fallimento, osserva, modifica, logga, loggamus)
import Core.Parsing (Parser)
import Eventi.Anagrafe (Anagrafe, esistenzaUtente, utenti, Responsabili, 
	esistenzaResponsabile, responsabili, validante, SUtente (..))
import Lib.Aspetti ((.<), see, ParteDi)
import Lib.Prioriti (R (..))
import Lib.Assocs (update , (?), upset)

import Voci.UI.Voci (ui)
import Voci.UI.Ordini (nuovoOrdine)
import Voci.Boxes (BoxVoce (..),BoxOrdine)

type Indice = QInteger

data Voce = Voce [String] [String] BoxVoce deriving (Eq, Show, Read)

data EsternoVoci = NuovaVoce Voce | EliminaVoce Voce deriving (Show,Read)

loggaVoce (Voce cs fs o) = render (singolare o) ++ "," ++ intercalate "," cs ++ "," ++ intercalate "," fs 
reazioneVoci :: (
	[Voce] `ParteDi` s,
	ParteDi Anagrafe s, 
	ParteDi Responsabili s,
	Parser c EsternoVoci
	) => Reazione s c Utente
reazioneVoci = soloEsterna reattoreVoci where
	reattoreVoci (first validante -> (wrap,NuovaVoce v)) = wrap $ \r -> do
		modifica (v:)
		loggamus $ "inserita nuova voce " ++ loggaVoce v	
		return (True,nessunEffetto)	
	reattoreVoci (first validante -> (wrap,EliminaVoce v)) = wrap $ \r -> do
		vs <- osserva
		fallimento (not $ v `elem` vs) $ "voce non presente"
		modifica $ filter (/= v)
		loggamus $ "eliminata la voce " ++ loggaVoce v	
		return (True,nessunEffetto)	

-- | costruttore di eventi per il modulo di accredito
costrEventiVoci :: (Functor m, Monad m, ParteDi [Voce] s) => CostrAction m c EsternoVoci s
costrEventiVoci s kp kn = 	[("definizione nuovo bene", runSupporto s kn kp $ callCC (nuovaVoce [] [] Nothing)),
				("eliminazione bene",elimaBene)
				] 
	where
	elimaBene = runSupporto s kn kp $ do
		vs <- asks see
		v <- scelte  (map (loggaVoce &&& id) vs) $ ResponseOne "bene da eliminare"
		return $ EliminaVoce v
	nuovaVoce cs fs co k =  do
		let c = Response [("categorie", ResponseMany $ map ResponseOne $ cs),
			("filiera", ResponseMany $  map ResponseOne $ fs),
			("unità minima di acquisto", ResponseOne $ maybe "nessuno" (render . singolare) co)]
		join $ scelte   [
			("aggiungi una categoria", addCat),
			("elimina una categoria", rmCat),
			("aggiungi un attore nella filiera", addFil),
			("elimina un attore dalla filiera", rmFil),
			("imposta l'unità minima di acquisto",setCom),
			("<fine>", case co of 
				Nothing -> do 
					output True $ ResponseOne "manca la definizione di unità minima di acquisto"
					nuovaVoce cs fs co k
				Just o -> k $ NuovaVoce $ Voce cs fs o)
			] c
		where
			addCat = libero  (ResponseOne "nuova categoria") >>= \c -> nuovaVoce (nub (c:cs)) fs co k
			addFil = libero  (ResponseOne "nuovo attore della filiera") >>= \c -> nuovaVoce cs (nub (c:fs)) co k
			rmCat = scelte  (map (id &&& id) cs)  (ResponseOne "selezione categoria da eliminare") >>= \c -> 
				nuovaVoce (filter (/=c) cs) fs co k
			rmFil = scelte  (map (id &&& id) fs) (ResponseOne "selezione attore da eliminare") >>= \c -> 
				nuovaVoce cs (filter (/=c) fs) co k
			setCom = toSupporto ui >>= \o -> nuovaVoce cs fs (Just o) k

costrQueryVoci :: (Monad m, [Voce] `ParteDi` s) => CostrAction m c Response s
costrQueryVoci s kp kn = [("beni inseribili negli acquisti", mostraBeni)] 
	where
	mostraBeni = runSupporto s kn kp $ do
		vs <- asks see
		Voce cs fs o <- scelte  (map (loggaVoce &&& id) vs) $ ResponseOne "scelta bene da mostrare"
		return $ Response [("categorie", ResponseMany $ map ResponseOne $ cs),
			("filiera", ResponseMany $  map ResponseOne $ fs),
			("unità minima di acquisto", ResponseOne . render . singolare $ o)
			]
	

