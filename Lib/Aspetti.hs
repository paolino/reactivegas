{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeOperators , OverlappingInstances#-}

-- | Implementazione del record dello stato , liberamente tratto dalle HList di Oleg. Contributo di Saizan.
-- Lo stato e' una 2-tupla nidificata nel secondo elemento. I tipi dei primi elementi sono tutti diversi e la selezione dell'elemento avviene attraverso la specificazione del tipo.

module Lib.Aspetti (ParteDi (..), seeset , (.<)) where

import Control.Applicative ((<$>)) 
import Control.Concurrent.STM (readTVar, writeTVar, atomically, newTVar)

-- | classe operativa dello stato, il primo parametro e' il tipo selezionato il secondo la struttura che lo contiene.
class ParteDi l ls where
	-- | estrae l'elemento di tipo l
	see :: ls -> l		
	-- | sostituisce l'elemento di tipo l
	set :: l -> ls -> ls	

instance ParteDi l (l,ls) where 
	see (l,_) = l 
	set l (_,ls) = (l,ls) 

instance ParteDi l ls => ParteDi l (l',ls) where 
	see (_,ls) = see ls
	set l (l',ls) = (l',set l ls)

instance ParteDi l l where
	see l = l
	set l l' = l

-- | modifica l'elemento di tipo l 
seeset 	:: ParteDi l ls 
	=> (l -> l) 	-- ^ modificatore 
	-> ls 		-- ^ struttura iniziale
	-> ls		-- ^ struttura finale
seeset f x =  set  (f $ see x) x


infixr 8 .<

-- | compositore di struttura, x <. y == (x,y)
(.<) :: l -> ls -> (l,ls)
(.<) = (,) 

infixr 8 :*:

-- | compositore di tipi 
type a :*: b = (a,b)



