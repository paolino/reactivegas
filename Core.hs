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
	parser :: String -> Maybe (c a) -- ^ dato un evento stringa si produce un potenziale valore taggato dal parser
	valore  :: c a -> a		-- ^ estrazione del valore dal tag 
	priorita :: c a -> Int		-- ^ priorita' di un evento nel fase di parsing 

-- |  parser interno utilizzato per la deserializzazione dello stato di controllo
--  attenzione alla relazione biiettiva show read in tutti gli eventi introdotti in tutti i plugin !!!!
data ParserConRead a = ParserConRead a 

-- | il parser standard che utilizza read e show, valido per qualsiasi evento
instance (Read a, Show a) =>  Parser ParserConRead a where
	parser s = ParserConRead <$> fst <$> listToMaybe (reads s)
	valore (ParserConRead a) = a
	priorita (ParserConRead a) = 0

-- readValore :: (Read a , Show a) => String -> Maybe a
-- readValore x = (valore :: ParserConRead  a -> a) <$> parser x

-- | monade di programmazione per le reazioni 
-- lo stato "s" dipende dall'applicazione , generalmente contine l'effetto degli eventi
-- "c" e' il parser scelto dall'applicazione
-- "d" e' un parametro libero che accompagna gli eventi, ie. l'utente
-- nella reader manteniamo le cause che conducono allo stato attuale, ovvero gli eventi che sono avvenuti, ma non possono essere dimenticati, in quanto il loro effetto non è ancora serializzabile
-- nella writer, i log, indicizzati per insieme di eventi causanti
newtype Inserzione s c d b = Inserzione (RWS [(d,String)] [([(d,String)],String)] s b) deriving (Functor, Monad, MonadState s, MonadReader [(d,String)],
		MonadWriter [([(d,String)],String)])

-- | una azione che associa un log all'insieme attuale di eventi causanti
logInserimento :: (MonadReader r m, MonadWriter [(r, t)] m) => t -> m ()
logInserimento x = do
	rs <- ask
	tell [(rs,x)]

-- | il runner per un Inserzione
runInserzione (Inserzione f) = runRWS  f

-- | gli effetti di un inserzione sono una lista di nuove reazioni e una lista di eventi interni, con la prima si aggiungono nuovi nodi all'albero delle reazioni
-- la seconda sono eventi che vengono subitaneamente prodotti
type Effetti s c d = ([Reazione s c d],[EventoInterno])

-- | inserzioni semplici che riducono il loro effetto nella modifica dello stato e/o nel log
nessunEffetto = ([],[]) :: Effetti s c d 

-- | Il tipo di una reazione parametrizzata su evento esterno e interno oltre che stato e parser
-- una TyReazione è una funzione che o da un evento interno o da un evento esterno produce una possibile Inserzione che ha come risultato un booleano che determina la persistenza della reazione e una serie di effetti da riferirsi come figli della razione stessa
type TyReazione a b d s c = ((Either b (d,a)) -> Inserzione s c d (Maybe (Bool, Effetti s c d)))

-- | una scatola esistenziale intorno ad un evento  che racchiude la sua potenziale trasformazione in evento 
data Deviatore c b = forall a . Parser c a => Deviatore (a -> Maybe b)

-- | il tipo che descrive l'operazione di accentrare molti deviatori sullo stesso evento interno (perche forzo il parser ?)
type Accentratore b = [Deviatore ParserConRead b]

-- | controlla se un deviatore accetta l'evento  e lo trasforma in un'altro
provaDeviatore :: forall b c . String -> Deviatore c b -> Maybe b
provaDeviatore x (Deviatore (f :: a -> Maybe b)) = (valore :: c a -> a) <$> parser x >>= f

-- | prova se un accentatore ha successo (da capire)
provaAccentratore :: String -> [Deviatore c a] -> Maybe a
provaAccentratore x = msum . map (provaDeviatore x)

-- | la scatola che nasconde i tipi degli eventi, notare che il tipo dell'evento interno richiede un parser specifico
-- una reazione incapsula un evento interno ed uno esterno che sono in alternativa grazie a TyReazione. Nel caso la reazione riguardi un evento interno
-- "b" e' possibile fornire anche un accentratore per l'evento stesso
data Reazione s c d = forall a b . (Parser c a , Parser ParserConRead b) => Reazione (Maybe (Accentratore b) , TyReazione a b d s c)

-- | wrappa una semplice azione in pura esterna
soloEsterna :: (Show d, Read d, Parser c a) => ((d,a) -> Inserzione s c d (Maybe (Bool, Effetti s c d))) -> Reazione s c d
soloEsterna f = Reazione (Nothing,either (\() -> return Nothing) f )

-- | Una scatola per gli eventi iterni
data EventoInterno = forall b . Parser ParserConRead b => EventoInterno b
--------------------------------------------------------------------------------------------------------

-- | serializzazione di evento contestualizzato allo stato di ingresso ,
-- esplicitiamo se l'evento é interno o esterno, per gli eventi interni non e' possibile associare il valore d
type Contestuale s d = (Either String (d,String) , s) 

-- | un nodo contiene una possible reazione, infatti le reazioni possono avere una vita limitata, e una lista di figli 
-- ognuno di essi contiene l'evento contestualizzato che lo ha creato e una lista di nodi indicizzati per intero (necessario ?)
data Nodo s c d = Nodo {
	reattore :: Maybe (Reazione s c d),
	seguenti :: [(Contestuale s d,[(Int,Nodo s c d)])] 
	}
-- | decora una serie di reazioni in nodi con seguenti nulli
mkNodi :: [Reazione s c d] -> [Nodo s c d]
mkNodi = map (flip Nodo [] . Just) 

-- | elimina i rami secchi, ovvero i nodi che non contengono reazioni e che non contengono seguenti
pruner :: Nodo s c d -> Nodo s c d
pruner (Nodo k rs) = Nodo k . 
	filter (not . null . snd) . -- elimina i gli eventi registrati che non contengono reazioni
	map (second $ 
		filter (\(_,Nodo k rs) -> isJust k || (not . null $ rs)) . -- elimina i nodi indicizzati morti
		map (second pruner) -- esegue il pruner sui nodi interni
		) 
	$ rs

-- | una monade temporanea che aiuta l'inserimento di un evento
-- la Writer accumula gli eventi interni creati contestualizzati alle loro cause
-- la SignalT dichiara che almeno un a reazione e' avvenuta
type Inserimento s c d = SignalT (WriterT [([(d,String)],EventoInterno)] (Inserzione s c d))
-- | inserisce un evento nello stato , causando una lista di eventi interni nella monade di WriterT 
inserimento :: Show d => (d,String) -> Nodo s c d -> SignalT (WriterT [([(d,String)],EventoInterno)] (Inserzione s c d)) (Nodo s c d)
-- l'inserimento quando attraversa un nodo privo di reazione, si propaga nei nodi inferiori
inserimento x (Nodo Nothing rs) = Nodo Nothing <$> mapM (secondM (mapM (secondM $ inserimento x)))  rs where
	secondM f (x,y) = f y >>= return . (,) x
inserimento (u,x) n@(Nodo k@(Just (Reazione (acc, f :: TyReazione a b d s c))) _) = do
	Nodo Nothing rs <- inserimento (u,x) n{reattore = Nothing} -- intanto eseguiamo l'inserzione nei figli simulando nodo morto, gancio al caso sopra
	s' <- get -- registriamo lo stato per un eventuale ripristino o per la contestualizzazione
	let 	complete v = do 
			let ctx = ((either (Left . show) (Right . second show) v), s') -- il contesto per gli eventuali nuovi figli
			result <- lift . lift $ f v -- esecuzione con creazione dei nodi dei reattori dipendenti
			case result of 
				Just (t, (zip [0..] . mkNodi -> ns, nevs)) -> do
					happened -- segnaliamo che almeno una reazione è avvenuta
					mapM logInserimento nevs -- logghiamo gli eventi interni eventualmente creati (logInserimento e' polimorfo :P ) 
					return . Nodo (if t then k else Nothing) $ (ctx , ns) : rs
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



