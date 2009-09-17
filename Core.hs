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
	parser :: Evento -> Maybe (c a) -- ^ dato un evento stringa si produce un potenziale valore taggato dal parser
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

-- | come ci si riferisce ad un evento
type Evento = String

-- | gli eventi prodotti dai reattori sono maneggiati come stringhe
type Interno = Evento

-- | gli eventi provenienti dall'esterno sono stringhe accoppiate con un valore di tipo libero per l'applicazione
type Esterno d = (d,Evento)


-- | Il contesto tiene conto degli eventi ai quali abbiamo reagito prima di trovarci a reagire all'ultimo. La catena ha sempre in testa un evento esterno, seguita da un numero qualsiasi di eventi interni
data Contesto d = Boot | Primo (Esterno d) | Oltre (Esterno d) [Interno] deriving Show

-- | il passaggio da un contesto all'altro deve seguire il protocollo : esterno, interno, interno, interno. ......
motiva :: Either Interno (Esterno d) -> Contesto d -> Contesto d
motiva (Right x) Boot = Primo x
motiva (Left x) (Primo y) = Oltre y [x]
motiva (Left x) (Oltre y xs) = Oltre y (xs ++ [x])
motiva _ _ = error "ricontestualizzazione fallita"

-- | I log dei reattori sono diretti all'utente 
type Message = String

-- | alcuni valori hanno senso solo insieme al loro contesto
type Contestualizzato d r = (Contesto d ,r)

-- | monade di programmazione per le reazioni 
-- lo stato "s" dipende dall'applicazione , generalmente contine l'effetto degli eventi
-- "c" e' il parser scelto dall'applicazione
-- "d" e' un parametro libero che accompagna gli eventi, ie. l'utente
-- nella reader manteniamo le cause che conducono allo stato attuale, ovvero gli eventi che sono avvenuti, ma non possono essere dimenticati, in quanto il loro effetto non è ancora serializzabile
-- nella writer, i log, indicizzati per insieme di eventi causanti

newtype Inserzione s c d b = Inserzione (RWS (Contesto d) [Contestualizzato d Message] s b) deriving 
	(Functor, Monad, MonadState s, MonadReader (Contesto d), MonadWriter [Contestualizzato d Message])

-- | una azione che associa un log all'insieme attuale di eventi causanti
logInserimento :: (MonadReader (Contesto d) m, MonadWriter [Contestualizzato d t] m) => t -> m ()
logInserimento x = ask >>= tell . return . flip (,) x

-- | il runner per un Inserzione
runInserzione (Inserzione f) = runRWS  f


-- | Una scatola per gli eventi iterni, permette ai reattori di essere polimorfi negli eventi prodotti
data EventoInterno = forall b . Parser ParserConRead b => EventoInterno b

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
provaDeviatore :: forall b c . Interno -> Deviatore c b -> Maybe b
provaDeviatore x (Deviatore (f :: a -> Maybe b)) = (valore :: c a -> a) <$> parser x >>= f

-- | prova se un accentatore ha successo (da capire)
provaAccentratore :: Interno -> [Deviatore c a] -> Maybe a
provaAccentratore x = msum . map (provaDeviatore x)

-- | la scatola che nasconde i tipi degli eventi, notare che il tipo dell'evento interno richiede un parser specifico
-- una reazione incapsula un evento interno ed uno esterno che sono in alternativa grazie a TyReazione. Nel caso la reazione riguardi un evento interno
-- "b" e' possibile fornire anche un accentratore per l'evento stesso
data Reazione s c d = forall a b . (Parser c a , Parser ParserConRead b) => Reazione (Maybe (Accentratore b) , TyReazione a b d s c)

-- | wrappa una semplice azione in pura esterna
soloEsterna :: (Show d, Read d, Parser c a) => ((d,a) -> Inserzione s c d (Maybe (Bool, Effetti s c d))) -> Reazione s c d
soloEsterna f = Reazione (Nothing,either (\() -> return Nothing) f )

--------------------------------------------------------------------------------------------------------

-- | serializzazione di evento contestualizzato allo stato di ingresso ,
-- esplicitiamo se l'evento é interno o esterno, per gli eventi interni non e' possibile associare il valore d
type Appuntato s d = (Either Interno (Esterno d) , s) 

-- | un nodo contiene una possible reazione, infatti le reazioni possono avere una vita limitata, e una lista di figli 
-- ognuno di essi contiene l'evento contestualizzato che lo ha creato e una lista di nodi indicizzati per intero (necessario ?)
data Nodo s c d = Nodo {
	reattore :: Maybe (Reazione s c d),
	seguenti :: [(Appuntato s d,[(Int,Nodo s c d)])] 
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
type Inserimento s c d = SignalT (WriterT [EventoInterno] (Inserzione s c d))
runInserimento = runWriterT . runSignalT
-- | inserisce un evento nello stato , causando una lista di eventi interni nella monade di WriterT 
inserimento :: Show d => Either Interno (Esterno d) -> Nodo s c d -> Inserimento s c d (Nodo s c d)
-- l'inserimento quando attraversa un nodo privo di reazione, si propaga nei nodi inferiori
inserimento x (Nodo Nothing rs) = Nodo Nothing <$> mapM (secondM (mapM (secondM $ inserimento x)))  rs where
	secondM f (x,y) = f y >>= return . (,) x
inserimento x n@(Nodo k@(Just (Reazione (acc, f :: TyReazione a b d s c))) _) = do
	Nodo Nothing rs <- inserimento x n{reattore = Nothing} -- intanto eseguiamo l'inserzione nei figli simulando nodo morto, gancio al caso sopra
	s' <- get -- registriamo lo stato per un eventuale ripristino o per la contestualizzazione
	let 	complete v = do 
			result <- lift . lift $ f v -- esecuzione con creazione dei nodi dei reattori dipendenti
			case result of 
				Just (t, (zip [0..] . mkNodi -> ns, nevs)) -> do
					happened -- segnaliamo che almeno una reazione è avvenuta
					tell nevs -- logghiamo gli eventi interni eventualmente creati 
					return . Nodo (if t then k else Nothing) $ ((x, s') , ns) : rs 
						--controlla se la reazione e' finita e aggiunge i nuovi reattori contestualizzati
				Nothing -> put s' >> rifiuto -- la reazione é fallita, lo stato viene ripristinato
		rifiuto = return (n{seguenti = rs}) 		
	case x of 
		Right (u,y) -> -- evento esterno
			case  (valore :: c a -> a) <$> parser y of 
				Nothing -> rifiuto -- non intercettato dai parser 
				Just v ->  complete (Right (u,v))
		Left y -> -- evento interno
			case maybe ((valore :: ParserConRead b -> b) <$> parser y) (provaAccentratore y) acc of 
				Just v -> complete (Left v)
				Nothing -> rifiuto

-- | l'evento interno del core segnala che nessun reattore ha accettato l'evento (parsing fallito)
data CoreEvents = CoreEventoRifiuto  deriving (Read,Show)

-- | un aiutante per costruire un reattore all'evento interno del core, evitando di esportare il costruttore non permettiamo ad altri  moduli di produrre l'evento, invece esportiamo una funzione per eseguire il pattern matching
eventoRifiutato :: CoreEvents -> Maybe ()
eventoRifiutato CoreEventoRifiuto = Just ()
eventoRifiutato _ = Nothing

-- | inserisce completamente un evento, reinserendo gli eventuali eventi interni creati durante l'inserimento stesso
inserimentoCompleto :: Show d => Esterno d -> [Nodo s c d] -> Inserzione s c d [Nodo s c d]
inserimentoCompleto x ns = fmap (fst . fst) . runInserimento  $ do	
		(ns',t) <- intercept $ consumaR ns (Right x) 
		if not t then  consumaR ns' $ Left [show CoreEventoRifiuto]
			else return ns'
	where 
	consuma :: Show d => [Nodo s c d] -> Either [Interno] (Esterno d) -> Inserimento s c d [Nodo s c d]
	consuma ns (Left xs) = foldM (\ns' x -> local (motiva $ Left x) $ inserimentoAlbero (Left x) ns')  ns xs
	consuma ns (Right e) = local (motiva $ Right e) $ inserimentoAlbero (Right e) ns
	
	inserimentoAlbero :: Show d => Either Interno (Esterno d) -> [Nodo s c d] -> Inserimento s c d [Nodo s c d]
	inserimentoAlbero x = mapM $ fmap pruner . inserimento x

	consumaR :: Show d => [Nodo s c d] -> Either [Interno] (Esterno d) -> Inserimento s c d [Nodo s c d]
	consumaR ns x = do
		(ns, map (\(EventoInterno e) -> show e) -> xs) <- listen $ consuma ns x
		if null xs then return ns else consumaR ns (Left xs)
