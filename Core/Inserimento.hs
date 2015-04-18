{-# LANGUAGE  ScopedTypeVariables, ViewPatterns, DeriveDataTypeable, NoMonomorphismRestriction , FlexibleContexts#-}


-- | modulo di trasformazione dello stato del programma. gli eventi sono espressi come stringhe mentre lo stato 
module Core.Inserimento where -- (nessunEffetto, TyReazione, Reazione (..), EventoInterno (..), Inserzione, ParserConRead, Parser) where

import Data.Typeable

import Control.Monad (foldM, mzero, when)
import Control.Monad.RWS (local,get, gets,lift,put, modify)
import Control.Monad.Writer (WriterT, runWriterT, tell, listen)
import Control.Applicative ((<$>))
import Control.Arrow (second)

import Control.Monad.Trans.Maybe (runMaybeT, MaybeT)

import Codec.Binary.UTF8.String -- (decodeString)

import Lib.Aspetti (see,seeset, ParteDi)
import Lib.Signal (SignalT, runSignalT, happened, intercept)

import Core.Parsing (valore, parser, ParserConRead)
import Core.Types (Interno,Esterno)
import Core.Contesto (motiva)
import Core.Programmazione (Fallimento (..), runInserzione, Message (..), EventoInterno (..), Inserzione, Reazione (..), TyReazione, provaAccentratore, logInserimento)
import Core.Nodo (Nodo (..), pruner, mkNodi)

import Debug.Trace
--------------------------------------------------------------------------------------------------------

-- | una monade temporanea che aiuta l'inserimento di un evento
-- la Writer accumula gli eventi interni creati contestualizzati alle loro cause
-- la SignalT dichiara che almeno un a reazione e' avvenuta
type Inserimento s c d = SignalT (WriterT [EventoInterno] (Inserzione s c d))
runInserimento = runWriterT . runSignalT


-- | inserisce un evento nello stato , tentando la reazione contenuda in un Nodo e quello in tutti i Nodi contenuti , causando una lista di eventi interni nella monade di WriterT 
inserimento 	:: Show d			--  il tag esterno deve essere serializzabile internamente
		=> Either Interno (Esterno d) 	-- ^ un evento interno o esterno da processare
		-> Nodo s c d 			-- ^ il ramo reattivo
		-> Inserimento s c d (Nodo s c d)	-- ^ il ramo reattivo aggiornato
-- l'inserimento quando attraversa un nodo privo di reazione, si propaga nei nodi inferiori
inserimento x (Nodo Nothing rs) = Nodo Nothing <$> mapM (secondM (mapM (secondM $ inserimento x)))  rs where
	secondM f (x,y) = f y >>= return . (,) x
inserimento x n@(Nodo k@(Just (Reazione (acc, f :: TyReazione a b d s c))) _) = do
	
	Nodo Nothing rs <- inserimento x n{reattore = Nothing} -- intanto eseguiamo l'inserzione nei figli simulando nodo morto, gancio al caso sopra
	s' <- get -- registriamo lo stato per un eventuale ripristino o per la contestualizzazione
	let 	complete v = do 
			
			result <- lift . lift $ f v -- esecuzione con creazione dei nodi dei reattori dipendenti
			case result of 
				Just (t {- condizione di mantenimento -}, (zip [0..] . mkNodi -> ns, nevs)) -> do
					happened -- segnaliamo che almeno una reazione è avvenuta
					tell nevs -- logghiamo gli eventi interni eventualmente creati 
					return . Nodo (if t then k else Nothing) $ ((x, s') , ns) : rs 
						--controlla se la reazione e' finita e aggiunge i nuovi reattori contestualizzati
				Nothing -> put s' >> rifiuto -- la reazione é fallita, lo stato viene ripristinato
		rifiuto = return (n{seguenti = rs}) -- il rifiuto non compromette l'eventuale accettazione dei sottonodi	
	case x of 
		Right (u,y) -> -- evento esterno
			case  (valore :: c a -> a) <$> parser y of 
				Nothing -> rifiuto -- non intercettato dal parser 
				Just v ->  complete (Right (u,v))
		Left y -> -- evento interno
			case maybe ((valore :: ParserConRead b -> b) <$> parser y) (provaAccentratore y) acc of 
				Nothing -> rifiuto
				Just v ->  complete (Left v)

-- | l'evento interno del core segnala che nessun reattore ha accettato l'evento (parsing fallito)
data CoreEvents = Rifiuto  deriving (Read,Show)

-- | un aiutante per costruire un reattore all'evento interno del core, evitando di esportare il costruttore non permettiamo ad altri  moduli di produrre l'evento, invece esportiamo una funzione per eseguire il pattern matching
eventoRifiutato :: CoreEvents -> Maybe ()
eventoRifiutato Rifiuto = Just ()
-- eventoRifiutato _ = Nothing

-- | inserisce completamente un evento, reinserendo gli eventuali eventi interni creati durante l'inserimento stesso
inserimentoCompleto :: Show d =>  [Nodo s c d] -> Esterno d -> Inserzione s c d (Maybe [Nodo s c d])
inserimentoCompleto ns x = fmap (fst . fst) . runInserimento  $ do	
		(ns',t) <- intercept $ consuma ns (Right x) 
		if not t then  return Nothing 
--			local (motiva $ Right x) . consumaR ns' $ Left [show Rifiuto]
			else return $ Just ns'
	where 	
	
	-- | esegue l'inserimento sui rami effettuando la pulizia dei sottorami secchi	
	inserimentoAlbero :: Show d => Either Interno (Esterno d) -> [Nodo s c d] -> Inserimento s c d [Nodo s c d]
	inserimentoAlbero x = mapM $ 
		fmap pruner .  -- pulizia dopo l'inserimento
		inserimento x  -- inserimento dell'evento interno o esterno
 
	-- | consuma un evento esterno oppure una lista di eventi interni
	consuma :: Show d => [Nodo s c d] -> Either [Interno] (Esterno d) -> Inserimento s c d [Nodo s c d]
	consuma ns (Left xs) = foldM f ns xs where
		f ns' x = local (motiva $ Left $ encodeString x) $ do
			(ns, map (\(EventoInterno e) -> show e) -> xs) <- listen $ inserimentoAlbero (Left x) ns'
			if null xs then return ns else consuma ns (Left xs)

	consuma ns (Right e) = local (motiva $ Right $ second encodeString e) $ do 
		(ns, map (\(EventoInterno e) -> show e) -> xs) <- listen $ inserimentoAlbero (Right e) ns
		if null xs then return ns else consuma ns (Left xs)

	-- | continua a consumare fino a che non vengono più prodotti eventi interni, 
	-- pericolo loop se i reattori sono rotti
	consumaR :: Show d => [Nodo s c d] -> Either [Interno] (Esterno d) -> Inserimento s c d [Nodo s c d]
	consumaR ns x = do
		(ns, map (\(EventoInterno e) -> show e) -> xs) <- listen $ consuma ns x
		if null xs then return ns else consumaR ns (Left xs)

------------------------ marasma ----------------------------
-- | la monade di inserimento completata con la gestione fallimento
type MTInserzione s c d = MaybeT (Inserzione s c d)

data UString  = UString String deriving Typeable

instance Show UString where
	show (UString s) = encodeString s

mus = Message . UString
-- | gestisce un fallimento segnalando al writer il motivo
fallimento :: Bool -> String -> MTInserzione s c d ()
fallimento t s = when t $ logga (Message . Fallimento . UString $ s) >> mzero

loggamus = logga . mus
logga s = lift (logInserimento s)
-- | legge il valore di tipo a dallo stato
osserva :: ParteDi a s => MTInserzione s c d a
osserva = lift $ gets see 

-- | modifica il valore di tipo a dallo stato
modifica :: ParteDi a s => (a -> a) -> MTInserzione s c d ()
modifica = lift . modify . seeset

-- | runner dello strato di gestione fallimento
conFallimento :: MTInserzione s c d a -> Inserzione s c d (Maybe a)
conFallimento = runMaybeT


