{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses, ScopedTypeVariables,FunctionalDependencies, 
 FlexibleInstances,  ExistentialQuantification , FlexibleContexts, Rank2Types, ViewPatterns, ImpredicativeTypes, NoMonomorphismRestriction #-}

-- | modulo di trasformazione dello stato del programma. gli eventi sono espressi come stringhe mentre lo stato 
module Core where -- (nessunEffetto, TyReazione, Reazione (..), EventoInterno (..), Inserzione, ParserConRead, Parser) where

import Control.Monad.RWS
import Control.Monad.Writer
import Control.Monad.State
import Control.Applicative
import Control.Arrow
import Data.List
import Data.Maybe
import Data.Either
import Debug.Trace

import Signal

-- | Un parser selezionabile 
class (Show a, Read a) => Parser c a where -- richiediamo Show qui per comoditá ....
	parser :: String -> Maybe (c a)
	valore  :: c a -> a
	priorita :: c a -> Int

--  parser interno utilizzato per la deserializzazione dello stato di controllo
--  attenzione alla relazione biiettiva show read in tutti gli eventi introdotti in tutti i plugin !!!!
data ParserConRead a = ParserConRead a 

instance (Read a, Show a) =>  Parser ParserConRead a where
	parser s = ParserConRead <$> fst <$> listToMaybe (reads s)
	valore (ParserConRead a) = a
	priorita (ParserConRead a) = 0

-- readValore :: (Read a , Show a) => String -> Maybe a
-- readValore x = (valore :: ParserConRead  a -> a) <$> parser x
-- | monade di programmazione per le reazioni , manca la reader che trasporti le configurazioni dei plugins
newtype Inserzione s c d b = Inserzione (RWS [(d,String)] [([(d,String)],String)] s b) deriving (Functor, Monad, MonadState s, MonadReader [(d,String)],
		MonadWriter [([(d,String)],String)])

logInserimento x = do
	rs <- ask
	tell [(rs,x)]

runInserzione (Inserzione f) = runRWS  f
type Effetti s c d = ([Reazione s c d],[EventoInterno])
nessunEffetto = ([],[]) :: Effetti s c d 

-- | Il tipo di una reazione parametrizzata su evento esterno e interno oltre che stato e parser
type TyReazione a b d s c = ((Either b (d,a)) -> Inserzione s c d (Maybe (Bool, Effetti s c d)))

-- | la scatola che nasconde i tipi degli eventi, notare che il tipo dell'evento interno richiede un parser specifico
data Reazione s c d = forall a b . (Parser c a , Parser ParserConRead b) => Reazione (Maybe (Accentratore b) , TyReazione a b d s c)

soloEsterna :: (Show d, Read d, Parser c a) => ((d,a) -> Inserzione s c d (Maybe (Bool, Effetti s c d))) -> Reazione s c d
soloEsterna f = Reazione (Nothing,either (\() -> return Nothing) f )

data EventoInterno = forall b . Parser ParserConRead b => EventoInterno b
--------------------------------------------------------------------------------------------------------

type Contestuale s d = (Either String (d,String) , s) -- serializzazione di evento contestualizzato allo stato di ingresso ,
	-- esplicitiamo se l'evento é interno o esterno
data Nodo s c d = Nodo {
	reattore :: Maybe (Reazione s c d),
	seguenti :: [(Contestuale s d,[(Int,Nodo s c d)])] 
	}
mkNodi :: [Reazione s c d] -> [Nodo s c d]
mkNodi = map (flip Nodo [] . Just) 

-- | elimina i rami secchi
pruner :: Nodo s c d -> Nodo s c d
pruner (Nodo k rs) = Nodo k . 
	filter (not . null . snd) . -- elimina i gli eventi registrati che non contengono reazioni
	map (second $ 
		filter (\(_,Nodo k rs) -> isJust k || (not . null $ rs)) . -- elimina i nodi indicizzati morti
		map (second pruner) -- esegue il pruner sui nodi interni
		) 
	$ rs

-- | inserisce un evento nello stato , causando una lista di eventi interni nella monade di WriterT 
inserimento :: Show d => (d,String) -> Nodo s c d -> SignalT (WriterT [([(d,String)],EventoInterno)] (Inserzione s c d)) (Nodo s c d)
-- l'inserimento quando attraversa un nodo privo di reazione, si propaga nei nodi inferiori
inserimento x (Nodo Nothing rs) = Nodo Nothing <$> mapM (secondM (mapM (secondM $ inserimento x)))  rs where
	secondM f (x,y) = f y >>= return . (,) x
inserimento (u,x) n@(Nodo k@(Just (Reazione (acc, f :: TyReazione a b d s c))) _) = do
	Nodo Nothing rs <- inserimento (u,x) n{reattore = Nothing} -- intanto proviamo nei figli simulando nodo morto
	let 	complete v = do 
			teta <- gets (\s zs -> (((either (Left . show) (Right . second show) v), s), zs): rs) -- una chiusura peri seguenti del nodo
			s' <- get -- registriamo lo stato per un eventuale ripristino
			result <- lift . lift $ f v -- esecuzione con creazione dei nodi dei reattori dipendenti
			case result of 
				Just (t, (mkNodi -> ns, nevs)) -> do
					rs <- lift ask
					happened
					tell (map ((,) rs) nevs) -- logghiamo gli eventi interni eventualmente creati
					return . Nodo (if t then k else Nothing) $ teta (zip [0..] ns)
				Nothing -> put s' >> rifiuto -- la reazione é fallita, lo stato viene ripristinato
		rifiuto = return (n{seguenti = rs}) 		
	case  (valore :: c a -> a) <$> parser x of 
		Nothing -> case maybe ((valore :: ParserConRead b -> b) <$> parser x) (provaAccentratore x) acc of 
			Nothing -> rifiuto -- non intercettato dai parser 
			Just v -> complete (Left v)
		Just v ->  complete (Right (u,v))

data CoreEvents = CoreEventoRifiuto  deriving (Read,Show)
eventoRifiutato CoreEventoRifiuto = Just ()
eventoRifiutato _ = Nothing

-- | inserisce completamente un evento, reinserendo gli eventuali eventi interni creati durante l'inserimento stesso
inserimentoCompleto :: Show d => (d,String) -> [Nodo s c d] -> Inserzione s c d [Nodo s c d]
inserimentoCompleto x@(u,_) ns = fst <$> fst <$> (runWriterT . runSignalT  $ k) where 
	k = do	(ns',t) <- intercept $ consumeWriter ns [([],x)]
		if not t then  consumeWriter ns' [([x],(u,show CoreEventoRifiuto))]
			else return ns'
	consumeWriter ns xs = do
		(ns, map (\(rs,EventoInterno e) -> (rs,(u,show e))) -> xs) <- listen $ cicloEventi ns xs
		if null xs then return ns else consumeWriter ns xs
	cicloEventi ns xs  = foldM (\ns' (rs,x) -> local (const $ rs ++[x]) $ mapM (\n -> pruner <$> inserimento x n) ns') ns xs


data Deviatore c b = forall b1 . Parser c b1 => Deviatore (b1 -> Maybe b)
type Accentratore b = [Deviatore ParserConRead b]

provaDeviatore :: forall b c . String -> Deviatore c b -> Maybe b
provaDeviatore x (Deviatore (f :: b1 -> Maybe b)) = (valore :: c b1 -> b1) <$> parser x >>= f

provaAccentratore x = msum . map (provaDeviatore x)
