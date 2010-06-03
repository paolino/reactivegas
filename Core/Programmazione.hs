{-# LANGUAGE ExistentialQuantification, ScopedTypeVariables, FlexibleContexts, GeneralizedNewtypeDeriving #-}
-- | monade di programmazione per le reazioni 
module Core.Programmazione where

import Data.Typeable
import Control.Monad.RWS (RWS, MonadState, MonadReader, MonadWriter, ask, tell, runRWS, msum)
import Control.Applicative ((<$>))

import Core.Types  (Interno)
import Core.Contesto (Contesto, Contestualizzato)
import Core.Parsing (ParserConRead, Parser, valore, parser)


data Message = forall a. (Show a, Typeable a) => Message a

instance Show Message where
	show (Message x) = show x
instance Eq Message where
	x == y = show x == show y

estrai :: Typeable a => [Contestualizzato d Message] -> ([a], [Contestualizzato d Message]) 
estrai [] = ([],[])
estrai (cm@(_,Message x):xs) = let 
	(ms,rs) = estrai xs
	in case cast x of
		Just z -> (z: ms, rs)
		Nothing -> (ms, cm:rs)

lascia :: Typeable a => a -> [Contestualizzato d Message] -> [Contestualizzato d Message]
lascia u [] = []
lascia u (cm@(_,Message x):xs) = let 
	rs = lascia u xs 
	in case typeOf x == typeOf u of
		True -> cm:rs
		False -> rs

-- | la monade
-- lo stato "s" dipende dall'applicazione , generalmente contine l'effetto degli eventi
-- "c" e' il parser scelto dall'applicazione che rimane libero ?
-- "d" e' un parametro libero che accompagna gli eventi, ie. l'utente
-- nella reader manteniamo le cause che conducono allo stato attuale, ovvero gli eventi che sono avvenuti, ma non possono essere dimenticati, in quanto il loro effetto non è ancora serializzabile
-- nella writer, i log, indicizzati per insieme di eventi causanti
newtype Inserzione s c d b = Inserzione (RWS (Contesto d) [Contestualizzato d Message] s b) deriving 
	(Functor, Monad, MonadState s, MonadReader (Contesto d), MonadWriter [Contestualizzato d Message])

-- | una azione che associa un log all'insieme attuale di eventi causanti
logInserimento :: (MonadReader (Contesto d) m, MonadWriter [Contestualizzato d t] m) => t -> m ()
logInserimento x = ask >>= tell . return . flip (,) x

-- | il runner per un Inserzione
runInserzione :: Inserzione s c d b -> Contesto d -> s -> (b, s, [Contestualizzato d Message])
runInserzione (Inserzione f) = runRWS  f

-- | Una scatola per gli eventi iterni, permette ai reattori di essere polimorfi negli eventi prodotti
data EventoInterno = forall b . Parser ParserConRead b => EventoInterno b

-- | gli effetti di un inserzione sono una lista di nuove reazioni e una lista di eventi interni, con la prima si aggiungono nuovi nodi all'albero delle reazioni
-- la seconda sono eventi che vengono subitaneamente prodotti
type Effetti s c d = ([Reazione s c d],[EventoInterno])

-- | inserzioni semplici che riducono il loro effetto nella modifica dello stato e/o nel log
nessunEffetto = ([],[]) :: Effetti s c d 

-- | Il risultato di una reazione è una azione nella monade di Inserzione, azione che restituisce Nothing in caso di dichiarato disinteresse , altrimenti un booleano che determina la persistenza della reazione e una serie di effetti da riferirsi come figli della razione stessa
type EfReazione s c d = Inserzione s c d (Maybe (Bool, Effetti s c d))
-- | Il tipo di una reazione parametrizzata su evento esterno e interno oltre che stato e parser
-- una TyReazione è una funzione che o da un evento interno o da un evento esterno produce una possibile Inserzione che ha come risultato un booleano che determina la persistenza della reazione e una serie di effetti da riferirsi come figli della razione stessa
type TyReazione a b d s c = (Either b (d,a)) -> EfReazione s c d


-- | wrappa una semplice azione in pura esterna
soloEsterna :: (Show d, Read d, Parser c a) => ((d,a) -> EfReazione s c d) -> Reazione s c d
soloEsterna f = Reazione (Nothing,either (\() -> return Nothing) f )

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

-- | una reazione incapsula un evento interno ed uno esterno che sono in alternativa grazie a TyReazione. Nel caso la reazione riguardi un evento interno
-- "b" e' possibile fornire anche un accentratore per l'evento stesso
data Reazione s c d = forall a b . (Parser c a , Parser ParserConRead b) => Reazione (Maybe (Accentratore b) , TyReazione a b d s c)


