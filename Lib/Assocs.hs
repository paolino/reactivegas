-- | alcune funzioni comode per la gestione delle liste associative
module Lib.Assocs where

import Data.List (lookup)
import Data.Maybe (isNothing)
-----------------------------------------

-- | aggiorna l'elemento selezionato fornendo un valore di default se l'elemento manca
update :: (Eq a) 	=> a		-- ^ indice 
			-> (b -> b) 	-- ^ modificatore
			-> b 		-- ^ valore di default
			-> [(a, b)] 	-- ^ lista iniziale
			-> [(a, b)]	-- ^ lista modificata
update k dv v kvs = case lookup k kvs of
	Nothing -> (k,dv v):kvs
	Just v -> (k,dv v):filter ((/=) k . fst) kvs

-- | imposta il valore dell'elemento selezionato
upset :: (Eq a) 	=> a		-- ^ indice 
			-> b		-- ^ valore di default
			-> [(a, b)]	-- ^ lista iniziale
			-> [(a, b)]	-- ^ lista modificata
upset x y = update x (const y) undefined

-- | controlla la assenza di un elemento
assente :: (Eq a) 	=> a 		-- ^ indice
			-> [(a, b)] 	-- ^ lista associativa
			-> Bool		-- ^ risultato della ricerca
assente x = isNothing . lookup x 

-- | elimina l'elemento selezionato
elimina :: (Eq a) 	=> a 		-- ^ indice
			-> [(a, b)] 	-- ^ lista iniziale
			-> [(a, b)]	-- ^ lista modificata
elimina k kvs = let (xs,ys) = break ((==k) . fst) kvs in xs ++ if null ys then ys else tail ys

-- | update monadico
updateM :: (Monad m ,Eq a) 	=> a		-- ^ indice 
				-> (b -> m b)	-- ^ modificatore
				-> b		-- ^ valore di default
				-> [(a,b)]	-- ^ lista iniziale
				-> m [(a,b)]	-- ^ lista modificata
updateM k dv v kvs = case lookup k kvs of
	Nothing -> dv v >>= \v' -> return $ (k,v'):kvs
	Just v -> dv v >>= \v' -> return $ (k,v'):filter ((/=) k . fst) kvs

-- | ricerca con default
(?) :: Eq a 	=> [(a,b)] 	-- ^ lista ricercata
		-> (a,b)	-- ^ indice accoppiato al valore di default
		-> b		-- ^ valore estratto
xs ? (k,t) = maybe t id $ lookup k xs 

