

{-# LANGUAGE NoMonomorphismRestriction, FlexibleInstances, FlexibleContexts, Rank2Types, ScopedTypeVariables, GADTs, ExistentialQuantification, StandaloneDeriving,UndecidableInstances, ViewPatterns, DeriveDataTypeable, MultiParamTypeClasses #-}
module Eventi.Voci where

import Data.List (nub, intercalate, partition, (\\), union, group, sort)
import Data.Maybe (catMaybes, fromJust)
import Data.Typeable 
import Control.Arrow ((&&&), first)
import Control.Monad (msum)
import Control.Monad.Cont (callCC, join, lift)
import Control.Monad.Reader (asks, ask)
import Control.Monad.Error (throwError)
import Lib.Units -- (Pesi,Volumi,Unità, Denaro,UnitClass (..))
import Lib.NaturalLanguage
import Lib.QInteger
import qualified Lib.Passo as P
import Eventi.Servizio
import Core.Costruzione -- (CostrAction)
import Lib.Response
import Core.Types -- (Utente)
import Core.Programmazione (Reazione (..), TyReazione, soloEsterna, nessunEffetto, Message (..))
import Core.Inserimento (MTInserzione, fallimento, osserva, modifica, logga, loggamus, conFallimento)
import Core.Parsing (Parser)
import Core.Dichiarazioni (Dichiarazione(Composta), Composta, Patch (..), holing)

import Eventi.Anagrafe (Anagrafe, esistenzaUtente, utenti, Responsabili, 
	esistenzaResponsabile, responsabili, validante, SUtente (..))
import Lib.Aspetti ((.<), see, ParteDi)
import Lib.Prioriti (R (..))
import Lib.Assocs (update , (?), upset)
import qualified Lib.Euro  as E

import Voci.UI.Voci (ui, modificaPrezzo)
import Voci.UI.Ordini (nuovoOrdine)
import Voci.Ordini (Valuta (..))
import Voci.Boxes (BoxVoce (..),BoxOrdine (..))
import Voci.Quantita (Quantità((:?)))

import qualified Lib.Units as U

import Debug.Trace


data Voce = Voce [String] [String] BoxVoce deriving (Eq, Show, Read)
data Ordine = Ordine [String] [String] BoxOrdine deriving (Eq, Show, Read)

denaro :: Ordine -> E.Euro
denaro q@(Ordine _ _ (BoxOrdine o)) = let x = valuta o :: Maybe (Quantità Denaro) in
	case x of 
		Just (y :? U.Euro) -> E.Euro y
		Just (y :? U.Centesimo) -> E.Euro (y/100)

data EventoVoci = CorreggiVoci [Voce] [Voce] | CorreggiAcquisto Indice [Voce] [Voce] 
	| CorreggiOrdine Indice Utente [Ordine] [Ordine] deriving (Typeable, Read, Show)

priorityEventoVoci = R k where	
	k (CorreggiVoci _ _) = -40
	k (CorreggiAcquisto _ _ _) = -39
	k (CorreggiOrdine _ _ _ _) = -39


data StatoVoci = StatoVoci {
	voci :: [Voce],
	acquisti :: [(Indice,(String, Utente,[Voce]))],
	ordini :: [((Indice,Utente),[Ordine])]
	} deriving (Show,Read,Eq)

mix xs xs' ys ys' = let
	xs'' = xs `union` xs'
	ys'' = ys `union` ys'
	in (xs'' \\ ys'', ys'' \\ xs'')

patcher (CorreggiVoci ns vs) (CorreggiVoci ns' vs') = Just $ uncurry CorreggiVoci $ mix ns ns' vs vs'
patcher (CorreggiAcquisto i ns vs) (CorreggiAcquisto i' ns' vs') 
	| i == i' = Just $ uncurry (CorreggiAcquisto i) $ mix ns ns' vs vs'
	| otherwise = Nothing
patcher (CorreggiOrdine i u ns vs) (CorreggiOrdine i' u' ns' vs') 
	| i == i' && u == u' = Just $ uncurry (CorreggiOrdine i u) $ mix ns ns' vs vs'
	| otherwise = Nothing
patcher _ _ = Nothing
nullaer (CorreggiVoci [] []) = True
nullaer (CorreggiAcquisto _ [] []) = True
nullaer (CorreggiOrdine _ _  [] []) = True
nullaer _ = False

data EventoInternoVoci = EventoAperturaImpegni Utente Indice String | EventoChiusuraImpegni Indice | InternoVoci EventoVoci deriving (Show,Read)

instance Patch EventoVoci where
	patch xs x = case msum . map (\(y,ys) -> (,) ys `fmap` patcher x y) $ holing xs of
		Nothing -> x : xs 
		Just (ys,y)  -> y : ys
	nulla = nullaer
data OrdiniChiusi = OrdiniChiusi Indice [((Indice,Utente),[Ordine])] deriving (Typeable,Show,Read)
loggaVoce (Voce cs fs o) = render (singolare o) ++ "," ++ intercalate "," cs ++ "," ++ intercalate "," fs 
loggaOrdine (Ordine cs fs o) = render (singolare o) ++ "," ++ intercalate "," cs ++ "," ++ intercalate "," fs 

reazioneVoci :: (
	StatoVoci `ParteDi` s,
	ParteDi Anagrafe s, 
	ParteDi Responsabili s,
	Parser c EventoVoci
	) => Reazione s c Utente
reazioneVoci = Reazione (Nothing,reattoreVoci) where
	reattoreVoci (Right (first validante -> (wrap,CorreggiVoci nvs evs))) = wrap $ \r -> do
		modifica $ \(StatoVoci vs as os) -> StatoVoci (nvs ++ (vs \\ evs)) as os
		loggamus $ "corretto l'insieme generale delle voci acquistabili "
		StatoVoci vs as os <- osserva
		return (True,nessunEffetto)
	reattoreVoci (Left (EventoAperturaImpegni u i s)) = conFallimento $ do
		modifica $ \(StatoVoci vs as os) -> StatoVoci vs ((i,(u,s,[])):as) os
		loggamus $ "aggiunto un nuovo acquisto al modulo ordini"
		return (True,nessunEffetto)
	reattoreVoci (Left (EventoChiusuraImpegni i)) = conFallimento $ do
		StatoVoci vs as os <- osserva
		case lookup i as of
			Nothing -> loggamus "l'acquisto non risulta nel modulo ordini"
			Just (u,_, _) -> do
				let (gs,os') = partition (\((j,_),_) -> i == j) os 
				logga $ Message (OrdiniChiusi i gs)
				modifica $ \(StatoVoci vs as os) -> StatoVoci vs (filter ((/=) i . fst) as) os'
				loggamus $ "modulo ordini relativo a " ++ show i
		return (True,nessunEffetto)
	reattoreVoci (Right (first validante -> (wrap,CorreggiAcquisto i nvs evs))) = wrap $ \r -> do
		StatoVoci vs as os  <- osserva
		case lookup i as of
			Nothing -> loggamus "l'acquisto non risulta nel modulo ordini"
			Just (u,s,vs) -> do
				fallimento (r /= u) "solo il responsabile d'acquisto può modificare le voci acquistabili"
				modifica $ \(StatoVoci dvs as os) -> 
						StatoVoci dvs ((i,(u,s,nvs ++ (vs \\ evs))) : filter ((/=) i . fst) as) os
				loggamus $ "elenco beni acquistabili in riferimento a " ++ show i ++ " modificato"
		return (True,nessunEffetto)
	reattoreVoci (Left (InternoVoci (CorreggiOrdine i u ns ves))) = conFallimento $ do
		StatoVoci vs as os <- osserva
		case lookup (i,u) os of
			Nothing -> do
				modifica $ \_ -> StatoVoci vs as $ ((i,u),ns):os
				loggamus $ "inserito nuovo ordine per " ++ show u ++ " in riferimento a " ++ show i
			Just ys -> do
				modifica $ \_ -> StatoVoci vs as $ ((i,u), ns ++ (ys \\ ves)):os 
				loggamus $ "modificato l'ordine di " ++ show u ++ " in riferimento a " ++ show i
		return (True,nessunEffetto)

-- | costruttore di eventi per il modulo di accredito
costrEventiVoci :: (Functor m,  Parser p EventoVoci,  Monad m, ParteDi StatoVoci s) => CostrAction m c (Dichiarazione p Composta) s
costrEventiVoci s kp kn = 	[("definizione nuova voce d'acquisto", runSupporto s kn kp $ callCC (nuovaVoce [] [] Nothing)),
				("clonazione voce d'acquisto", clonaVoce),
				("correzione voce d'acquisto", correggiVoce),
				("eliminazione voce d'acquisto",elimaBene),
				("assegnazione voce ad un acquisto",assegnaVoce),
				("eliminazione voce da un acquisto",staccaVoce)
				] 
	where	

	assegnaVoce = runSupporto s kn kp $ do
		StatoVoci vs as os <- asks see
		i <- scelte (map (\(i,(u,s,vs)) -> (s,i)) as) $ ResponseOne "selezione acquisto da modificare"
		StatoVoci vs as os <- asks see
		let r = lookup i as
		case r of
			Nothing -> throwError "acquisto inesistente"
			Just (u,s,vs') -> do 
				v <- scelte (map (loggaVoce &&& id) (vs \\ vs')) $ ResponseOne $ "voce da inserire nell'acquisto " ++ s	
				return . Composta $ [CorreggiAcquisto i [v] []]
	staccaVoce = runSupporto s kn kp $ do
		StatoVoci vs as os <- asks see
		(i,vs') <- scelte (map (\(i,(u,s,vs)) -> (s,(i,vs))) as) $ 
			ResponseOne "selezione acquisto da modificare"
		StatoVoci vs as os <- asks see
		let r = lookup i as
		case r of
			Nothing -> throwError "acquisto inesistente"
			Just (u,s,vs') -> do 
				v <- scelte (map (loggaVoce &&& id) (vs')) $ ResponseOne $ "voce da togliere dall'acquisto " ++ s
				return . Composta $ [CorreggiAcquisto i [] [v]]

	elimaBene = runSupporto s kn kp $ do
		StatoVoci vs _ _ <- asks see
		v <- scelte  (map (loggaVoce &&& id) vs) $ ResponseOne "voce d'acquisto da eliminare"
		return . Composta $ [CorreggiVoci [] [v]]
	clonaVoce = runSupporto s kn kp $ do
			StatoVoci vs _ _ <- asks see
			Voce cs fs o <- scelte  (map (loggaVoce &&& id) vs) $ ResponseOne "voce d'acquisto da clonare" 
			v' <- callCC $ correggiVoce' cs fs o
			return . Composta $ [CorreggiVoci [v'] []]
	correggiVoce = runSupporto s kn kp $ do
			StatoVoci vs _ _ <- asks see
			v@(Voce cs fs o) <- scelte  (map (loggaVoce &&& id) vs) $ ResponseOne "voce d'acquisto da clonare" 
			v' <- callCC $ correggiVoce' cs fs o
			return . Composta $ [CorreggiVoci [v'] [v]]

	correggiVoce' cs fs o k =  do 
				let c = Response [("categorie", ResponseMany $ map ResponseOne $ cs),
					("filiera", ResponseMany $  map ResponseOne $ fs),
					("unità minima di acquisto", ResponseOne . render . singolare $ o)]
				join $ scelte   [
					("aggiungi una categoria", addCat),
					("elimina una categoria", rmCat),
					("aggiungi un attore nella filiera", addFil),
					("elimina un attore dalla filiera", rmFil),
					("correggi il prezzo",setCom),
					("<fine>", k $ Voce cs fs o)
					] c
			where
			addCat = libero  (ResponseOne "correggi categoria") >>= \c -> correggiVoce' (nub (c:cs)) fs o k
			addFil = libero  (ResponseOne "nuovo attore della filiera") >>= \c -> correggiVoce' cs (nub (c:fs)) o k
			rmCat = scelte  (map (id &&& id) cs)  (ResponseOne "selezione categoria da eliminare") >>= \c -> 
				correggiVoce' (filter (/=c) cs) fs o k
			rmFil = scelte  (map (id &&& id) fs) (ResponseOne "selezione attore da eliminare") >>= \c -> 
				correggiVoce' cs (filter (/=c) fs) o k
			setCom = toSupporto (modificaPrezzo o) >>= \o -> correggiVoce' cs fs o k

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
				Just o -> k . Composta $ [CorreggiVoci [Voce cs fs o] []])
			] c
		where
			addCat = libero  (ResponseOne "nuova categoria") >>= \c -> nuovaVoce (nub (c:cs)) fs co k
			addFil = libero  (ResponseOne "nuovo attore della filiera") >>= \c -> nuovaVoce cs (nub (c:fs)) co k
			rmCat = scelte  (map (id &&& id) cs)  (ResponseOne "selezione categoria da eliminare") >>= \c -> 
				nuovaVoce (filter (/=c) cs) fs co k
			rmFil = scelte  (map (id &&& id) fs) (ResponseOne "selezione attore da eliminare") >>= \c -> 
				nuovaVoce cs (filter (/=c) fs) co k
			setCom = toSupporto ui >>= \o -> nuovaVoce cs fs (Just o) k
mostraBeni s kp kn =  runSupporto s kn kp $ do
	StatoVoci vs as os <- asks see
	Voce cs fs o <- scelte  (map (loggaVoce &&& id) vs) $ ResponseOne "scelta bene da mostrare"
	return $ Response [("categorie", ResponseMany $ map ResponseOne $ cs),
		("filiera", ResponseMany $  map ResponseOne $ fs),
		("unità minima di acquisto", ResponseOne . render . singolare $ o)
		]
mostraAcquisti i s kp kn =  runSupporto s kn kp $ do
	StatoVoci _ as _ <- asks see
	(i,s,u,vs) <- case lookup i as of
		Nothing -> throwError "acquisto non disponibile"
		Just (u,s,vs) -> return (i,s,u,vs)
	Voce cs fs o <- scelte  (map (loggaVoce &&& id) vs) $ Response $ [
		("nome acquisto",ResponseOne s),
		("riferimento acquisto",ResponseOne i),
		("responsabile acquisto",ResponseOne u),
		("voci acquistabili",ResponseOne "")
		]
	return $ Response [("categorie", ResponseMany $ map ResponseOne $ cs),
		("filiera", ResponseMany $  map ResponseOne $ fs),
		("unità minima di acquisto", ResponseOne . render . singolare $ o)
		]
mostraOrdini i u s kp kn = runSupporto s kn kp $ do
	StatoVoci _ as os <- asks see
	s <- case lookup i as of
		Nothing -> throwError "acquisto non disponibile"
		Just (u,s,vs) -> return s
	case lookup (i,u) os of
		Nothing -> throwError $ "l'utente " ++ u ++ " non ha ancora effettuato un ordine per " ++ show s
		Just xs -> do 
			Ordine cs fs o <- scelte (map (loggaOrdine &&& id) xs) $ ResponseOne $ "selezione ordine di " ++ u ++ " sull'acquisto " ++ s
			return $ Response [
					("categorie", ResponseMany $ map ResponseOne $ cs),
					("filiera", ResponseMany $  map ResponseOne $ fs),
					("quantità acquistata in denaro", ResponseOne . render . singolare $ o)
					]

costrQueryVoci :: (Monad m, StatoVoci `ParteDi` s) => CostrAction m c Response s
costrQueryVoci s kp kn = [("beni inseribili negli acquisti", mostraBeni),("acquisti aperti con beni", mostraAcquisti),("ordini", mostraOrdini)] 
	where
	mostraBeni = runSupporto s kn kp $ do
		StatoVoci vs as os <- asks see
		Voce cs fs o <- scelte  (map (loggaVoce &&& id) vs) $ ResponseOne "scelta bene da mostrare"
		return $ Response [("categorie", ResponseMany $ map ResponseOne $ cs),
			("filiera", ResponseMany $  map ResponseOne $ fs),
			("unità minima di acquisto", ResponseOne . render . singolare $ o)
			]
	mostraAcquisti = runSupporto s kn kp $ do
		StatoVoci _ as _ <- asks see
		(i,s,u,vs) <- scelte (map (\(i,(u,s,vs)) -> (s,(i,s,u,vs))) as) $ 
			ResponseOne "selezione acquisto da mostrare"
		Voce cs fs o <- scelte  (map (loggaVoce &&& id) vs) $ Response $ [
			("nome acquisto",ResponseOne s),
			("riferimento acquisto",ResponseOne i),
			("responsabile acquisto",ResponseOne u),
			("voci acquistabili",ResponseOne "")
			]
		return $ Response [("categorie", ResponseMany $ map ResponseOne $ cs),
			("filiera", ResponseMany $  map ResponseOne $ fs),
			("unità minima di acquisto", ResponseOne . render . singolare $ o)
			]
	mostraOrdini = runSupporto s kn kp $ do
		StatoVoci _ as os <- asks see
		(i,s) <- scelte (map (\(i,(_,s,_)) -> (s,(i,s))) as) $ 
			ResponseOne "selezione acquisto"
		let us = map head . group . sort . map (\((_,u),_) -> u) . filter (\((j,_),_) -> j == i) $ os
		u <- scelte (map (id &&& id) us) $ ResponseOne "selezione utente"
		case lookup (i,u) os of
			Nothing -> throwError $ "l'utente " ++ u ++ " non ha ancora effettuato un ordine per " ++ s
			Just xs -> do 
				Ordine cs fs o <- scelte (map (loggaOrdine &&& id) xs) $ ResponseOne "selezione ordine da mostrare"
				return $ Response [
						("categorie", ResponseMany $ map ResponseOne $ cs),
						("filiera", ResponseMany $  map ResponseOne $ fs),
						("quantità acquistata", ResponseOne . render . singolare $ o)
						]



