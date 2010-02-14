module Core.Nodo (
	Appuntato,
	Nodo (..),
	mkNodi,
	pruner
	)
	where

import Control.Arrow (second)
import Data.Maybe (isJust)

import Core.Types (Interno,Esterno)
import Core.Programmazione (Reazione)


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

